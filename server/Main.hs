{-# LANGUAGE BangPatterns, DeriveDataTypeable, ScopedTypeVariables,
             TypeFamilies, PatternGuards, CPP #-}
{-# OPTIONS_GHC -Wnot #-}
-- |
-- Module      : Scion.Server.Emacs
-- License     : BSD-style
--
-- Maintainer  : marco-oweber@gmx.de
-- Stability   : pre-alpha
-- Portability : portable
--
-- An example server which will talk to different backends
-- The first handshake is done this way:
-- The client sends : "select scion-server protocol: name version"
-- where name and version specify the protocol to be used.
-- the server replies in any case by either
-- "ok\n" or "failure : message\n"
-- From then on the specific protocol handler takes over control
--
-- multiple connections to the same server are not yet supported
-- because I don't know yet in detail when ghc api calls can be made
-- concurrently.. Maybe using an MVar is an option (TODO)

module Main where

import MonadUtils ( liftIO )
import Scion.Server.Generic as Gen
--import qualified Scion.Server.ProtocolEmacs as Emacs
import qualified Scion.Server.ConnectionIO as CIO
import Scion (runScion)
import qualified Scion.Types  as ST


import Prelude hiding ( log )
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.IO (stdin, stdout, hSetBuffering, hFlush, BufferMode(..))
import qualified System.Log.Logger as HL
import qualified System.Log.Handler.Simple as HL
import qualified System.Log.Handler.Syslog as HL
import qualified Data.ByteString.Lazy.Char8 as S
import Network ( listenOn, PortID(..) )
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.BSD
import Network.Socket.ByteString
import Data.List (isPrefixOf, break)
import Data.Foldable (foldrM)
import qualified Control.Exception as E
import Control.Monad ( when, forever, liftM )
import Control.Concurrent (threadDelay)
import System.Console.GetOpt

log = HL.logM __FILE__
logInfo = log HL.INFO
logDebug = log HL.DEBUG
logError = log HL.ERROR

-- how should the client connect to the server? 
-- if you're paranoid about your code Socketfile or StdInOut
-- will be the most secure choice.. (Everyone can connect via TCP/IP at the
-- moment)
data ConnectionMode = Server Bool PortNumber -- the Bool indicates whether to scan
                  | Client HostName PortNumber
                  | StdInOut
#ifndef mingw32_HOST_OS
                  | Socketfile FilePath
#endif
  deriving Show

data StartupConfig = StartupConfig {
     connectionMode :: ConnectionMode
   , portNumber :: PortNumber
   , connectHost :: HostName
   , autoPort :: Bool
   , showHelp :: Bool
} deriving Show

-- | Default port to which scion-server listens in server mode
defaultPort = 4005
-- | Default host to which scion-server connects in client mode
defaultHost = "localhost"
-- | Default start configuration is server mode

defaultStartupConfig = StartupConfig (Server False (fromInteger defaultPort))
                                     (fromInteger defaultPort)
                                     defaultHost
                                     False
                                     False

options :: [OptDescr (StartupConfig -> IO StartupConfig)]
options =
     [ Option ['p']     ["port"]
       (ReqArg (\o opts -> return $ opts { portNumber = (fromInteger (read o)) }) (show defaultPort))
       "listen on or connect to this TCP port"
     , Option ['a'] ["autoport"]
       (NoArg (\opts -> return $ opts { autoPort = True }))
       "scan until a free TCP port is found"
     , Option ['i'] ["stdinout"]
       (NoArg (\opts -> return $ opts { connectionMode = StdInOut}))
       "client must connect to stdin and stdout"
#ifndef mingw32_HOST_OS
     , Option ['s'] ["socketfile"]
       (ReqArg (\o opts -> return $ opts { connectionMode = Socketfile o})
               "/tmp/scion-io")
       "listen on this socketfile"
#endif
     , Option ['h'] ["help"] (NoArg (\opts -> return $ opts { showHelp = True } ))
              "show this help"

     , Option ['f'] ["log-file"] (ReqArg (\f opts -> do
          fh <- HL.fileHandler f HL.DEBUG
          HL.updateGlobalLogger "" (HL.addHandler fh)
          return opts ) "/tmp/scion-log") "log to the given file"
     , Option ['c'] ["client"]
       (NoArg (\opts -> return $ opts { connectionMode = (Client (connectHost opts) (portNumber opts))}))
       "Operate scion-server in client mode."
     , Option ['n'] ["hostname"]
       (ReqArg (\o opts -> return $ opts { connectHost = o }) defaultHost)
       "Host name or address to which scion-server should connect in client mode."
     ]

initializeLogging = do
  -- by default log everything to stdout
  HL.updateGlobalLogger "" (HL.setLevel HL.DEBUG)

helpText = do
    pN <- getProgName
    let header = unlines [ "usage of scion server (executable :"  ++ pN  ++ ")" ]
    return $ usageInfo header options

-- attempts to listen on each port in the list in turn, and returns the first successful
listenOnOneOf :: [PortID] -> IO Socket
listenOnOneOf (p:ps) = catch
    (listenOn p)
    (\(ex :: IOError) -> if null ps then E.throwIO ex else listenOnOneOf ps)

-- this way, we can iterate until we find a free port number
instance Bounded PortNumber where
    minBound = 0
    maxBound = 0xFFFF

serve :: ConnectionMode -> IO ()
serve (Server auto nr) = do
  sock <- liftIO $ if auto
                   then listenOnOneOf (map PortNumber [nr..maxBound])
                   else listenOn (PortNumber nr)
  realNr <- liftIO $ socketPort sock
  realAddr<- liftIO $ getSocketName sock
  logInfo $ "=== Listening on port: " ++ show realNr
  logInfo $ "=== Listening on address: " ++ show realAddr
  hFlush stdout
  let run True  = return ()
      run False = 
       E.handle (\(e::E.IOException) -> do
                    logInfo ("caught :" ++ (show e) ++ "\n\nwaiting for next client")
                    run False) $ do
         (sock', _addr) <- liftIO $ accept sock
         sock_conn <- CIO.mkSocketConnection sock'
         stop_server <- handleClient sock_conn
         run stop_server
  run False

serve (Client host port) = 
  let
    maxRetries = 5
    initialSleep = 1000000 `div` 2

    -- Iterate through all of the candidate addresses and see if one of them
    -- actually connects. If none of the connections succeeds, return Nothing
    getSocket []           = return Nothing
    getSocket (addr:addrs) =
      E.bracketOnError
          (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
          (sClose)  -- only done if there's an error
          (\sock -> E.catch (do connect sock (addrAddress addr)
                                return (Just sock))
                            (\(e :: E.IOException) -> getSocket addrs))

    -- The workhorse: Try to connect to the candidate addresses, and if none of
    -- them succeed, sleep using an exponential backoff timer, and try again.
    tryConnect addrs =
      let
        tryConnect' addrs retry sleep
          | retry < maxRetries = do
              sock <- getSocket addrs
              case sock of
                -- Exponential backoff
                Nothing -> do
                  threadDelay sleep
                  tryConnect' addrs (retry + 1) (sleep * 2)
                -- Got something...
                (Just s) -> do
                  a <- getSocketName s
                  logInfo $ "client mode connected to " ++ (show a) ++ " (retries " ++ (show retry) ++ ")"
                  return sock

          | otherwise = return Nothing
      in
        tryConnect' addrs 0 initialSleep

    -- Run the server
    run _ True  = return ()
    run sock False = do
      sock_conn <- CIO.mkSocketConnection sock
      stop_server <- handleClient sock_conn
      run sock stop_server
  in do
    -- This was lifted from the Network package, because Network.connectTo returns a
    -- Handle, not a Socket. We need a Socket.
    proto <- getProtocolNumber "tcp"
    let hints = defaultHints { addrFlags = [AI_ADDRCONFIG]
                             , addrProtocol = proto
                             , addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just host) (Just (show port))
    logInfo $ "client mode candidate addresses: " ++ (show $ map addrAddress addrs)
    r <- tryConnect addrs
    case r of
      Nothing     -> do
        logInfo $ "Unable to connect to any candidate address"
        return ()

      (Just sock) -> run sock False

serve StdInOut = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  _ <- handleClient (stdin, stdout)
  return ()

#ifndef mingw32_HOST_OS
serve (Socketfile file) = do
  sock <- liftIO $ listenOn (UnixSocket file)
  let run True = return ()
      run _ =
       E.handle (\(e::E.IOException) -> do
                    logInfo ("caught :" ++ (show e) ++ "\n\nwaiting for next client")
                    run False) $ do
         (sock', _addr) <- liftIO $ accept sock
         sock_conn <- CIO.mkSocketConnection sock'
         stop_server <- handleClient sock_conn
         run stop_server
  run False
#endif

-- does the handshaking and then runs the protocol implementation 
handleClient :: (CIO.ConnectionIO con) => con -> IO Bool
handleClient con = do
  runScion $ Gen.handle con 0

fixConfig :: StartupConfig -> StartupConfig
fixConfig conf = case connectionMode conf of
  Server _ _ -> conf { connectionMode = Server (autoPort conf) (portNumber conf) }
  Client _ _ -> conf { connectionMode = Client (connectHost conf) (portNumber conf) }
  otherwise -> conf

main :: IO ()
main = do

  -- logging 
  initializeLogging

  -- cmd opts 
  (opts, nonOpts, _err_msgs) <- fmap (getOpt Permute options) getArgs

  when ((not . null) nonOpts) $
    logError $ "no additional arguments expected, got: " ++ (show nonOpts)

  startupConfig <- return . fixConfig =<< foldrM ($) defaultStartupConfig opts

  -- help 
  when (showHelp startupConfig) $ helpText >>= putStrLn >> exitSuccess

  -- start server 
  logInfo "starting server"
  -- E.handle (\(e :: E.SomeException) ->
  --             do
  --               logInfo ("shutting down server due to exception "  ++ show e)) $
  do
    log HL.DEBUG $ "opts: " ++ (show startupConfig)
    serve (connectionMode startupConfig)
