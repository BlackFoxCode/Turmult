{-# LANGUAGE OverloadedStrings  #-}
{- |
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>

-}

module Discord.Gateway
  ( runDiscord
  )
where

import           Discord.Gateway.Types
import           Discord.Gateway.Exceptions

import           Control.Concurrent
import           Control.Concurrent.Async       (race)
import           Control.Concurrent.STM.TChan
import           Control.Exception.Safe
import           Control.Lens
import           Data.Aeson                     ( Result(..)
                                                , fromJSON
                                                , toJSON
                                                , encode
                                                )
import qualified Data.ByteString.Char8         as B
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
import           System.Info                    (os)
import           Wuss

type TokenAuth = Text
type SessionID = Text
type SeqenceID = Int
data LoopState = LoopStart
               | LoopClosed
               | LoopReconnect TokenAuth SessionID SeqenceID
               deriving (Show)

data ConnData = ConnData
    { connection :: Connection
    , sessionID  :: SessionID
    , auth       :: TokenAuth
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
        let intents = defaultIntents { _gateway_intents_guild_messages = True, _gateway_intents_guild_message_typing = True }
        let ident = Identify token (Properties (toText os) "hDiscord" "hDiscord") (Just False) Nothing Nothing (Just $ Presence Nothing [] Invisible False) Nothing intents
        sendTextData conn . encode $ Payload 2 (Just $ toJSON ident) Nothing Nothing
        isReady <- receiveDataMessage conn
        let ready :: Ready
            ready = fromJust $ case fromJSON . fromJust $ fromDataMessage isReady ^. payload_d of
              Success x -> Just x
              _         -> Nothing
        putStrLn "Starting Event Loop"
        startEventLoop conn token (_ready_session_id ready) hello
      case next :: Either ConnectionException LoopState of
        Left (CloseRequest n t) -> putStrLn $ "Closing connection on request: " <> show n <> " - " <> show t
        Left err                -> putStrLn $ "Error in Gateway Loop: " <> show err
        Right n                 -> print n >> outer n
    LoopClosed       -> pass
    LoopReconnect {} -> pass

startEventLoop :: Connection -> TokenAuth -> SessionID -> Hello -> IO LoopState
startEventLoop conn token sessID hello = do
    sendChan <- newTChanIO :: IO (TChan Payload)
    seqID <- newTVarIO 0 :: IO (TVar Int)
    let err :: SomeException -> IO LoopState
        err _ = do
            sid <- readTVarIO seqID
            return $ LoopReconnect token sessID sid

    handle err $ do
        heartbeatT <- forkIO $ heartbeat sendChan seqID (_hello_heartbeat_interval hello)
        sendmsgT   <- forkIO $ sendLoop conn sendChan

        finally (eventLoop (ConnData conn sessID token) seqID (_hello_heartbeat_interval hello))
                (killThread heartbeatT >> killThread sendmsgT)

eventLoop :: ConnData -> TVar Int -> Int -> IO LoopState
eventLoop connData seqID interval = loop
 where
  loop :: IO LoopState
  loop = do
    msg <- race (threadDelay (interval * 1000)) $ do
      d <- receiveDataMessage (connection connData)
      return $ fromDataMessage d
    payload <- case msg of
      Left  () -> pure $ Left NoResponse -- Timed out
      Right x  -> pure $ Right x         -- Leave as is
    print payload
    case (payload :: Either GWConnException Payload) of
      Left  UnexpectedResponse -> LoopReconnect (auth connData) (sessionID connData) <$> readTVarIO seqID
      Left  NoResponse         -> loop -- Just keep going, for now
      Right (Payload 0 d _ t)  -> do -- Dispatch Event - Should be handled by user code
        putStrLn $ "Got Dispatch: " <> toString (fromJust t)
        putStrLn $ "Data: " <> show d
        loop
      Right (Payload 1 _ _ _) -> do -- Heartbeat Request
        putStrLn "Got Heartbeat Request"
        seqID' <- readTVarIO seqID
        sendTextData (connection connData) $ Payload 1 (Just $ toJSON (Just seqID' :: Heartbeat)) Nothing Nothing
        loop
      Right (Payload 7 d _ _) -> do -- Reconnect
        let shouldResume :: Reconnect
            shouldResume = case fromJSON $ fromJust d of
              Success x -> x
              _         -> False
        if shouldResume then LoopReconnect (auth connData) (sessionID connData) <$> readTVarIO seqID else return LoopClosed
      Right (Payload 9 d _ _) -> do -- Invalid Session
        let resumable :: InvalidSession
            resumable = case fromJSON $ fromJust d of
              Success x -> x
              _         -> False
        if resumable then LoopReconnect (auth connData) (sessionID connData) <$> readTVarIO seqID else return LoopClosed
      Right (Payload 11 _ _ _) -> -- Heartbeat ACK
          loop -- Should monitor this
      Right p -> do -- We shouldn't receive any of the other Op codes.
          putStrLn $ "Unexepected Op Code: " <> show p
          loop


heartbeat :: TChan Payload -> TVar Int -> Int -> IO ()
heartbeat chan seqID interval = do
    threadDelay 1000000 -- 1 second
    forever $ do
      num <- readTVarIO seqID
      let hb = Payload 1 (Just $ toJSON (Just num :: Heartbeat)) Nothing Nothing
      atomically $ writeTChan chan hb
      putStrLn "<HeartBeat>"
      threadDelay $ interval * 1000 -- Convert interval to microseconds

sendLoop :: Connection -> TChan Payload-> IO ()
sendLoop conn chan = waitLoop
    where
        delay    = round (10^(6 :: Int) * (60 / 100) :: Double) :: Int -- Send about 100 events per minute
        waitLoop = do
            res <- atomically $ tryReadTChan chan
            case res of
              Just msg -> sendTextData conn (encode msg) >> waitLoop
              Nothing  -> threadDelay delay >> waitLoop
