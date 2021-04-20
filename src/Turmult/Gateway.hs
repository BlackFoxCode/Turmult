{-# LANGUAGE LambdaCase         #-}
{- |
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>

-}

module Turmult.Gateway
  ( runDiscord
  )
where

import           Turmult.Gateway.Types

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Exception.Safe
import           Control.Lens
import           Data.Aeson                     ( Result(..)
                                                , fromJSON
                                                , toJSON
                                                , encode
                                                )
import qualified Data.ByteString.Char8         as B
import           Data.Default                   ( def )
import           Data.Maybe                     ( fromJust )
import           Network.HTTP.Req               ( (/:)
                                                , GET(..)
                                                , NoReqBody(..)
                                                , defaultHttpConfig
                                                , header
                                                , https
                                                , jsonResponse
                                                , responseBody
                                                , req
                                                , runReq
                                                )
import           Network.WebSockets
import           System.Info                    ( os )
import           Wuss                           ( runSecureClient )

type SessionID = Text
type SeqenceID = Int
data LoopState = LoopStart
               | LoopClosed
               | LoopReconnect SessionID SeqenceID
               deriving (Show)

data ConnData = ConnData
    { connection :: Connection
    , sessionID  :: SessionID
    }

getGatewayInfo :: Text -> IO GatewayInfo
getGatewayInfo token = do
  r <-
    runReq defaultHttpConfig
    $ do
        req GET (https "discord.com" /: "api" /: "gateway" /: "bot") NoReqBody jsonResponse
    $ header "Authorization" (B.pack ("Bot " <> toString token))
  liftIO . return $ responseBody r


openGateway :: Text -> GatewayParams -> ClientApp a -> IO a
openGateway token params app = do
    info <- getGatewayInfo token
    let
      host = drop 6 $ toString (info ^. url) -- Drop 'wss://'
      port = 443 -- The Discord gateway always uses a secure websocket
      path = "/?v=" <> show (params ^. version) <> "&encoding=" <> show (params ^. encoding) <> maybe
        ""
        (\x -> "?compress=" <> show x)
        (params ^. compress)
      in runSecureClient host port path app

runDiscord :: Text -> GatewayParams -> IO ()
runDiscord token params = outer LoopStart
 where
  outer :: LoopState -> IO ()
  outer s = case s of
    LoopStart -> do
      next <- try $ openGateway token params $ \conn -> do
        msg <- receiveDataMessage conn
        let hello :: Hello
            hello = fromJust $ case fromJSON . fromJust $ fromDataMessage msg ^. payload_d of
              Success x -> Just x
              _         -> Nothing
        let ident = Identify token
                             (Properties (toText os) "hDiscord" "hDiscord")
                             (Just False) -- Compress
                             (Just 50)    -- Max members before server won't send offline users
                             Nothing      -- Shard info
                             Nothing      -- Presence
                             Nothing      -- Guild Subscriptions
                             def          -- Default Intents
        sendTextData conn . encode $ Payload 2 (Just $ toJSON ident) Nothing Nothing
        isReady <- receiveDataMessage conn
        let ready :: Ready
            ready = fromJust $ case fromJSON . fromJust $ fromDataMessage isReady ^. payload_d of
              Success x -> Just x
              _         -> Nothing
        putStrLn "Starting Event Loop"
        startEventLoop conn (_ready_session_id ready) hello
      case next :: Either ConnectionException LoopState of
        Left  (CloseRequest n t) -> putStrLn $ "Closing connection on request: " <> show n <> " - " <> show t
        Left  err                -> putStrLn $ "Error in Gateway Loop: " <> show err
        Right n                  -> print n >> outer n
    LoopClosed                 -> pass
    LoopReconnect sessID seqID -> do
      next <- try $ openGateway token params $ \conn -> do
        msg <- receiveDataMessage conn
        let hello :: Hello
            hello = fromJust $ case fromJSON . fromJust $ fromDataMessage msg ^. payload_d of
              Success x -> Just x
              _         -> Nothing
        let resume = Resume token sessID seqID
        sendTextData conn . encode $ Payload 6 (Just $ toJSON resume) Nothing Nothing
        isReady <- receiveDataMessage conn
        let ready :: Ready
            ready = fromJust $ case fromJSON . fromJust $ fromDataMessage isReady ^. payload_d of
              Success x -> Just x
              _         -> Nothing
        putStrLn "Starting Event Loop"
        startEventLoop conn (_ready_session_id ready) hello
      case next :: Either ConnectionException LoopState of
        Left  (CloseRequest n t) -> putStrLn $ "Closing connection on request: " <> show n <> " - " <> show t
        Left  err                -> putStrLn $ "Error in Gateway Loop: " <> show err
        Right n                  -> print n >> outer n

startEventLoop :: Connection -> SessionID -> Hello -> IO LoopState
startEventLoop conn sessID hello = do
  sendChan <- newTChanIO :: IO (TChan Payload)
  seqID    <- newTVarIO 0 :: IO (TVar Int)
  let err :: SomeException -> IO LoopState
      err _ = do
        sid <- readTVarIO seqID
        return $ LoopReconnect sessID sid

  handle err $ do
    heartbeatT <- forkIO $ heartbeat sendChan seqID (_hello_heartbeat_interval hello)
    sendmsgT   <- forkIO $ sendLoop conn sendChan

    finally (eventLoop (ConnData conn sessID) seqID (_hello_heartbeat_interval hello))
            (killThread heartbeatT >> killThread sendmsgT)

eventLoop :: ConnData -> TVar Int -> Int -> IO LoopState
eventLoop connData seqID _ = loop
 where
  loop :: IO LoopState
  loop = do
    msg <- receiveDataMessage (connection connData)
    case fromDataMessage msg of
      Payload 0 d _ t  -> do -- Dispatch Event - Should be handled by user code
        putStrLn $ "Got Dispatch: " <> toString (fromJust t)
        putStrLn $ "Data: " <> show d
        loop
      Payload 1 _ _ _ -> do -- Heartbeat Request
        putStrLn "Got Heartbeat Request"
        seqID' <- readTVarIO seqID
        sendTextData (connection connData) $ Payload 1 (Just . toJSON $ Just seqID') Nothing Nothing
        loop
      Payload 7 d _ _ -> do -- Reconnect
        let shouldResume :: Reconnect
            shouldResume = case fromJSON $ fromJust d of
              Success x -> x
              _         -> False
        if shouldResume
          then LoopReconnect (sessionID connData) <$> readTVarIO seqID
          else return LoopClosed
      Payload 9 d _ _ -> do -- Invalid Session
        let resumable :: InvalidSession
            resumable = case fromJSON $ fromJust d of
              Success x -> x
              _         -> False
        if resumable then LoopReconnect (sessionID connData) <$> readTVarIO seqID else return LoopClosed
      Payload 11 _ _ _ -> -- Heartbeat ACK
        loop -- Should monitor this
      other -> do -- We shouldn't receive any of the other Op codes.
        putStrLn $ "Unhandled Op Code: " <> show other
        loop

heartbeat :: TChan Payload -> TVar Int -> Int -> IO ()
heartbeat chan seqID interval = forever $ do
    threadDelay $ interval * 1000 -- Convert interval to microseconds
    num <- readTVarIO seqID
    let payload = Payload 1 (Just $ toJSON (Just num :: Heartbeat)) Nothing Nothing
    atomically $ writeTChan chan payload
    putStrLn "<HeartBeat>"

sendLoop :: Connection -> TChan Payload -> IO ()
sendLoop conn chan = forever $ do
      msg <- atomically (tryReadTChan chan)
      when (isJust msg) $ forM_ msg (sendTextData conn . encode)
      threadDelay $ round (10 ^ (6 :: Int) * (60 / 100) :: Double)
      -- Discord allows 120 messages per minute. We'll stay at 100/m for now

