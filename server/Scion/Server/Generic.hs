module Scion.Server.Generic 
  ( handle
  ) where

import Prelude hiding ( log )

import qualified Data.Map as DM

import Scion
import Scion.Types (gets, SessionState(..))
import qualified Scion.Types.JSONDictionary as Dic
import Scion.Server.ConnectionIO as CIO
import Scion.Server.Commands

import Text.JSON.AttoJSON
--import Text.JSON.Types
import qualified Data.ByteString.Char8 as S
--import qualified Data.ByteString.UTF8 as S
import qualified System.Log.Logger as HL

--import System.CPUTime
--import Text.Printf

log :: HL.Priority -> String -> IO ()
log = HL.logM "protocol.generic"
logDebug :: MonadIO m => String -> m ()
logDebug = liftIO . log HL.DEBUG

type StopServer = Bool

handle :: (ConnectionIO con) =>
          con
       -> Int
       -> ScionM StopServer
handle con 0 = do
   loop
  where
   loop = do
     -- TODO: don't require line-based input
     --start <- liftIO $ getCPUTime
     str <- liftIO $ CIO.getLine con
     --logDebug $ "==> " ++ S.toString str
     --readT <- liftIO $ getCPUTime
     let mb_req = parseJSON  str
     --decodeT <- liftIO $ getCPUTime
     (resp, keep_going) 
         <- case mb_req of
              Left _ -> return (malformedRequest, True)
              Right req -> do
                --logDebug $ "Cmd: " ++ show req
                handleRequest req
     --processT <- liftIO $ getCPUTime           
     c <- gets client
     --let resp_str = encodeStrict (if (c == "vim") then vimHack resp else resp)
     let resp_str= showJSON (if (c == "vim") then vimHack resp else resp)
     --encodeT <- liftIO $ getCPUTime   
     --logDebug $ "<== " ++ resp_str
     --let bs=(S.fromString resp_str)
     --bsT <- liftIO $ getCPUTime
     liftIO $ CIO.putLine con resp_str
     --end <- liftIO $ getCPUTime
     --logDebug $ (printf "<==> Read time: %0.3f sec\n" (((fromIntegral (readT - start)) / (10^12)) :: Double))
     --logDebug $ (printf "<==> Decode time: %0.3f sec\n" (((fromIntegral (decodeT - readT)) / (10^12)) :: Double))
     --logDebug $ (printf "<==> Process time: %0.3f sec\n" (((fromIntegral (processT - readT)) / (10^12)) :: Double))
     --logDebug $ (printf "<==> Encode time: %0.3f sec\n" (((fromIntegral (encodeT - processT)) / (10^12)) :: Double))
     --logDebug $ (printf "<==> To ByteString time: %0.3f sec\n" (((fromIntegral (bsT - encodeT)) / (10^12)) :: Double))
     --logDebug $ (printf "<==> Write time: %0.3f sec\n" (((fromIntegral (end - encodeT)) / (10^12)) :: Double))
     --logDebug $ (printf "<==> Full time: %0.3f sec\n" (((fromIntegral (end - start)) / (10^12)) :: Double))
     --logDebug $ "sent response"
     if keep_going then loop else do 
       --logDebug "finished serving connection."
       return True

handle con unknownVersion = do
  -- handshake failure, don't accept this client version 
  liftIO $ CIO.putLine con $ 
    S.pack $ "failure: Don't know how to talk to client version "
      ++ (show unknownVersion)
  return False

-- vim doesn't know about true,false,null thus can't parse it. this functions
-- mapps those values to 1,0,""
vimHack :: JSValue -> JSValue
vimHack JSNull = JSString Dic.empty
vimHack (JSBool True) = JSNumber 1
vimHack (JSBool False) = JSNumber 0
vimHack (JSArray l) = JSArray $ map vimHack l
vimHack (JSObject m) = JSObject $ DM.map vimHack m
vimHack e = e  -- JSRational, JSString 
