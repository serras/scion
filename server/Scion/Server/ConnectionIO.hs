{-# LANGUAGE FlexibleInstances, ExistentialQuantification, MultiParamTypeClasses #-}
-- |
-- Module      : Scion.Server.ConnectionIO
-- License     : BSD-style
--
-- Maintainer  : marco-oweber@gmx.de
-- Stability   : experimental
-- Portability : portable
--
-- Abstraction over Socket and Handle IO.

module Scion.Server.ConnectionIO (
  ConnectionIO(..), mkSocketConnection
) where

import qualified Scion.Types.JSONDictionary as Dic
import Prelude hiding (log)
import System.IO (Handle, hFlush)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, send)
import Data.IORef
{- FIXME: unused import qualified System.Log.Logger as HL -}
import qualified Data.ByteString.Char8 as S

{- FIXME: unused
log :: HL.Priority -> String -> IO ()
log = HL.logM "io.connection"

logError :: String -> IO ()
logError = log HL.ERROR -}

class ConnectionIO con where
  getLine :: con -> IO S.ByteString
  getN    :: con -> Int -> IO S.ByteString
  put     :: con -> S.ByteString -> IO ()
  putLine :: con -> S.ByteString -> IO ()
  putLine c s = put c s >> put c Dic.newline -- (S.singleton '\n')

-- (stdin,stdout) implemenation 
instance ConnectionIO (Handle, Handle) where
  getLine (i, _) = do l <- S.hGetLine i; return l --(L.fromChunks [l])
  getN (i,_) = S.hGet i
  put (_,o) = S.hPut o
  putLine (_,o) = \l -> do
      -- ghc doesn't use the ghc api to print texts all the time. So mark
      -- scion replies by a leading "scion:" see README.markdown
      S.hPutStr o Dic.scionPrefix
      S.hPut o l
      S.hPutStr o Dic.newline
      hFlush o -- don't ask me why this is needed. LineBuffering is set as well (!) 


data SocketConnection = SockConn Socket (IORef S.ByteString)

mkSocketConnection :: Socket -> IO SocketConnection
mkSocketConnection sock = 
    do r <- newIORef S.empty; return $ SockConn sock r

-- Socket.ByteString implementation 
instance ConnectionIO SocketConnection where
  -- TODO: Handle client side closing of connection.
  getLine (SockConn sock r) = do 
      buf <- readIORef r
      (line_chunks, buf') <- go buf
      writeIORef r buf'
      return (S.concat line_chunks)
    where
      go buf | S.null buf = do
        chunk <- recv sock 1024
        if S.null chunk
         then return ([], S.empty)
         else go chunk
      go buf =
          let (before, rest) = S.breakSubstring Dic.newline buf in
          case () of
           _ | S.null rest -> do
               -- no newline found
               (cs', buf') <- go rest
               return (before:cs', buf')
           _ | otherwise ->
               return ([before], S.drop (S.length Dic.newline) rest)

  getN (SockConn sock r) len = do
      buf <- readIORef r
      if S.length buf > len
       then do let (str, buf') = S.splitAt len buf
               writeIORef r buf'
               return str --(L.fromChunks [str])
       else do
         str <- recv sock (len - S.length buf)
         writeIORef r S.empty
         return (S.concat [buf, str])

  put (SockConn sock _) str = do
  --    go lstr --(L.toChunks lstr)
      -- is there a better excption which should be thrown instead?  (TODO)
      -- throw $ mkIOError ResourceBusy ("put in " ++ __FILE__) Nothing Nothing
 --  where go [] = return ()
  --       go (str:strs) = do
           --let l = S.length str
           --sent <- send sock str
           send sock str
           return()
           --if (sent /= l) then do
            -- logError $ (show l) ++ " bytes to be sent but could only sent : " ++ (show sent)
           --else return() -- go strs
