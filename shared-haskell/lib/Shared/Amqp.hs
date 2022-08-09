{-# LANGUAGE StrictData, ScopedTypeVariables, OverloadedStrings #-}
{- HLINT ignore "Reduce duplication" -}

module Shared.Amqp
  ( module Shared.Amqp
  , module Shared.Messages
  , Message
  ) where

import           Control.Concurrent             ( MVar
                                                , forkIO
                                                , newEmptyMVar
                                                , newMVar
                                                , putMVar
                                                , readMVar
                                                , takeMVar
                                                )
import           Control.Exception              ( SomeException
                                                , throwIO
                                                , try
                                                )
import           Control.Monad                  ( void
                                                , when
                                                )
import           Control.Retry                  ( RetryStatus )
import           Data.Aeson                    as A
                                                ( eitherDecode
                                                , encode
                                                )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as C8
import           Data.Functor                   ( (<&>) )
import qualified Data.HashMap.Strict           as Hm
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TLE
import           Data.Time                      ( getCurrentTime )
import           Data.Time.Clock.POSIX          ( utcTimeToPOSIXSeconds )
import           Data.UUID                      ( toText )
import           Data.UUID.V4                   ( nextRandom )
import           GHC.Stack                      ( HasCallStack )
import           Network.AMQP                   ( Ack(Ack)
                                                , Channel
                                                , ConnectionOpts(coName)
                                                , ConsumerTag
                                                , DeliveryMode(Persistent)
                                                , Envelope(envRoutingKey)
                                                , Message(..)
                                                , QueueOpts(..)
                                                , ackEnv
                                                , addChannelExceptionHandler
                                                , addConnectionClosedHandler
                                                , consumeMsgs
                                                , declareQueue
                                                , fromURI
                                                , newMsg
                                                , newQueue
                                                , openChannel
                                                , openConnection''
                                                , publishMsg
                                                , qos
                                                )
import           Prelude                 hiding ( id )
import           Shared.Messages                ( CommandMessage(..)
                                                , EventMessage(..)
                                                , MessageBody(..)
                                                , ReplyFailure(..)
                                                , ReplyMessage(..)
                                                , ReplySuccess(..)
                                                , TgthrMessage(..)
                                                , createTgthrMsg
                                                , defaultTgthrMsg
                                                , failureWithText
                                                , genSuccess
                                                , userFromReply
                                                )
import           Shared.TgthrMessages.Base      ( MessageID
                                                , ThroughMQ(toKey)
                                                , fromMessageID
                                                )
import           System.Exit                    ( ExitCode(ExitFailure) )
import           System.Posix.Process           ( exitImmediately )
import           System.Timeout                 ( timeout )
---------------------------------------------------------------

type CorrMap = Hm.HashMap Text (MVar (Message, Envelope))
type Syncro = MVar CorrMap
newtype OpaqueChannel = OpaqueChannel (Channel, Text, Syncro)
type RawCallback a = Text -> (BL.ByteString, Message) -> IO a
type Callback a = OpaqueChannel -> Text -> (TgthrMessage, Message) -> IO a
type SentMessage = TgthrMessage
type ReceivedMessage = TgthrMessage
type ParentMessage = TgthrMessage
type ParentMessageID = MessageID

--- HELPERS ---
lazyBStoStrictText :: BL.ByteString -> Text
lazyBStoStrictText = TL.toStrict . TLE.decodeUtf8

-- | `returnTgthrMsg` creates a new Tgthr Message with random uuid and current time
returnTgthrMsg :: Maybe ParentMessage -> MessageBody -> IO TgthrMessage
returnTgthrMsg parentMsg msgContents = do
  uuid <- nextRandom
  now  <- getCurrentTime
  return $ createTgthrMsg (uuid, now) parentMsg msgContents

createRawMsgWithId :: TgthrMessage -> Maybe Network.AMQP.Message
createRawMsgWithId msg = Just $ newMsg
  { msgID          = Just . toText . fromMessageID . tgthrMsgid $ msg
  , msgContentType = Just "application/json"
  }

getAMQPChan :: OpaqueChannel -> Channel
getAMQPChan (OpaqueChannel (chan, _, _)) = chan

closedHandler :: IO ()
closedHandler = do
  putStrLn "Error: AMQP Connection closed..."
  exitImmediately $ ExitFailure 1

channelExceptionHandler :: SomeException -> IO ()
channelExceptionHandler e = do
  putStr "Error: AMQP Channel exception: " >> print e
  exitImmediately $ ExitFailure 1

startChannel :: Maybe Text -> String -> IO OpaqueChannel
startChannel appName uri = do
  let connectionOptions = (fromURI uri) { coName = appName }
      opts = newQueue { queueAutoDelete = True, queueExclusive = True }
  conn        <- openConnection'' connectionOptions
  _           <- addConnectionClosedHandler conn True closedHandler
  chan        <- openChannel conn
  _           <- addChannelExceptionHandler chan channelExceptionHandler
  _           <- qos chan 0 5 False -- max 5  messages unacked
  syncro      <- newMVar Hm.empty
  (rqn, _, _) <- declareQueue chan opts

  _           <- consumeMsgs chan rqn Ack $ wakeUpRPCThread syncro
  return $ OpaqueChannel (chan, rqn, syncro)

wakeUpRPCThread :: Syncro -> (Message, Envelope) -> IO ()
wakeUpRPCThread syncro (msg, env) = do
  let corrId = msgCorrelationID msg
  case corrId of
    Nothing -> do
      putStr $ "No msgCorrelationID for: " <> show msg
      ackEnv env
      return ()
    Just id -> do
      currentCorrIds <- takeMVar syncro
      case Hm.lookup id currentCorrIds of
        Nothing -> do
          putStr $ "No response for: " <> show msg
          ackEnv env
          return ()
        Just var -> do
          putMVar var (msg, env)
          putMVar syncro $ Hm.delete id currentCorrIds

replyToRawMsg
  :: OpaqueChannel -> Message -> BL.ByteString -> Maybe Message -> IO ()
replyToRawMsg (OpaqueChannel (channel, _, _)) msg msgContents settings = do
  let rqnMaybe = msgReplyTo msg
      cid      = msgCorrelationID msg
  now <- floor . (* 1000) . utcTimeToPOSIXSeconds <$> getCurrentTime
  let finalMsg = baseMsg { msgBody          = msgContents
                         , msgDeliveryMode  = Just Persistent
                         , msgCorrelationID = cid
                         , msgTimestamp     = Just now
                         }
  -- putStrLn
  --   $  "Replying to: "
  --   <> show (msgCorrelationID msg)
  --   <> " with >> "
  --   <> show finalMsg
  --   <> " <<"
  case rqnMaybe of
    Nothing  -> return ()
    Just rqn -> void $ publishMsg channel "" rqn finalMsg
  where baseMsg = fromMaybe newMsg settings

publishRawMsg
  :: OpaqueChannel -> Text -> BL.ByteString -> Maybe Message -> IO ()
publishRawMsg (OpaqueChannel (channel, _, _)) key msgContents settings = do
  now <- floor . (* 1000) . utcTimeToPOSIXSeconds <$> getCurrentTime
  let finalMsg = baseMsg { msgBody         = msgContents
                         , msgDeliveryMode = Just Persistent
                         , msgTimestamp    = Just now
                         }
  -- putStrLn $ "Publishing: " <> show key <> " >> " <> show finalMsg <> " << "
  void $ publishMsg channel "tgthr" key finalMsg
  where baseMsg = fromMaybe newMsg settings

publishRawMsgWithResponse
  :: OpaqueChannel -> Text -> BL.ByteString -> Maybe Message -> IO BL.ByteString
publishRawMsgWithResponse (OpaqueChannel (channel, rqn, syncro)) key msgContents settings
  = do
    cid <- toText <$> nextRandom
    now <- floor . (* 1000) . utcTimeToPOSIXSeconds <$> getCurrentTime
    let finalMsg = baseMsg { msgCorrelationID = Just cid
                           , msgReplyTo       = Just rqn
                           , msgBody          = msgContents
                           , msgTimestamp     = Just now
                           }
    -- putStrLn
    --   $  "Publishing: "
    --   <> show key
    --   <> "with >> "
    --   <> show finalMsg
    --   <> " <<"
    _              <- publishMsg channel "tgthr" key finalMsg
    msgSync        <- newEmptyMVar
    currentCorrIds <- takeMVar syncro
    _              <- putMVar syncro $ Hm.insert cid msgSync currentCorrIds
    (msg, env)     <- readMVar msgSync
    _              <- ackEnv env
    return $ msgBody msg
  where baseMsg = fromMaybe newMsg settings

replyToMsg
  :: HasCallStack
  => OpaqueChannel
  -> Message
  -> ParentMessage
  -> MessageBody
  -> IO ()
replyToMsg _ _ _ (EventV1 _) =
  error "replyToMsg => Does not support that type of message"
replyToMsg _ _ _ (CommandV1 _) =
  error "replyToMsg => Does not support that type of message"
replyToMsg chan msg parentMsg msgContents = do
  tgthrMsg <- returnTgthrMsg (Just parentMsg) msgContents
  publishRawMsg chan
                (toKey msgContents)
                (encode tgthrMsg)
                (createRawMsgWithId tgthrMsg)
  replyToRawMsg chan msg (encode tgthrMsg) . Just $ newMsg
    { msgID          = Just . toText . fromMessageID . tgthrMsgid $ tgthrMsg
    , msgContentType = Just "application/json"
    }

publish
  :: OpaqueChannel -> Maybe ParentMessage -> MessageBody -> IO SentMessage
publish chan parentMsg msgContents = do
  msg <- returnTgthrMsg parentMsg msgContents
  publishRawMsg chan (toKey msgContents) (encode msg) (createRawMsgWithId msg)
  return msg

publishWithReply'
  :: HasCallStack
  => OpaqueChannel
  -> Maybe ParentMessage
  -> MessageBody
  -> Text
  -> IO (ReceivedMessage, SentMessage)
publishWithReply' chan parentMsg msgContents key = do
  msg   <- returnTgthrMsg parentMsg msgContents
  reply <- publishRawMsgWithResponse chan
                                     key
                                     (encode msg)
                                     (createRawMsgWithId msg)
  -- case decode reply of
  --   Just x  -> putStr "Reply: " >> print (tgthrBody x)
  --   Nothing -> putStr "Decode failed on: " >> print reply
  let decoded = eitherDecode reply
  case decoded of
    Right m -> return (m, msg)
    Left  e -> do
      putStr "publishWithReply': " >> print e
      print reply
      error e

-- | The 'publishWithReply' returns (response, message that was sent)
publishWithReply
  :: OpaqueChannel
  -> Maybe ParentMessage
  -> MessageBody
  -> IO (ReceivedMessage, SentMessage)
publishWithReply chan parentMsg msgContents =
  publishWithReply' chan parentMsg msgContents $ toKey msgContents

type QueueName = Text
type AMQPPublisher = CommandMessage -> IO MessageBody
type CallbackResults = (Maybe ReplyMessage, [EventMessage])
type AMQPCallback
  = AMQPPublisher -> ParentMessageID -> TgthrMessage -> IO CallbackResults
addConsumer :: QueueName -> AMQPCallback -> OpaqueChannel -> IO ConsumerTag
addConsumer aQueueName callback chan =
  addRawConsumer chan aQueueName $ \key (msgContents, msg) -> do
    let decodedTgthrMsg = eitherDecode msgContents
    case decodedTgthrMsg of
      Left e -> do
        putStr ("Could not decode: " <> C8.unpack msgContents <> " error: ")
          >> print e
        replyToRawMsg chan msg (C8.pack e) Nothing
      Right (tgthrMsg :: TgthrMessage) -> do
        let
          replyWith   = replyToMsg chan msg tgthrMsg
          publishWith = publish chan $ Just tgthrMsg
          pReplyWith m =
            publishWithReply chan (Just tgthrMsg) (CommandV1 m)
              <&> (tgthrBody . fst)
          messageId = tgthrMsgid tgthrMsg
        result :: Either SomeException CallbackResults <- try
          (callback pReplyWith messageId tgthrMsg)
        case result of
          Left _ -> putStr "Error: callback result: "
            >> print (tgthrMsgid tgthrMsg, key, result)
          Right (Nothing, []) -> return ()
          Right _             -> return () --putStr "Callback result: " >> print result

        case result of
          Left err ->
            replyWith
              . ReplyV1
              . ReplyFailureV1
              . GenFailureText
              . T.pack
              $ show err

          Right (reply, events) -> do
            let hasElemt = not . null
            when (hasElemt events) (print (tgthrBody tgthrMsg, events))
            case reply of
              Just replyMsg -> replyWith $ ReplyV1 replyMsg
              Nothing       -> return ()
            mapM_ (publishWith . EventV1) events

addRawConsumer :: OpaqueChannel -> Text -> RawCallback a -> IO ConsumerTag
addRawConsumer (OpaqueChannel (chan, _, _)) aQueueName callback =
  consumeMsgs chan aQueueName Ack $ \(msg, env) -> (void . forkIO) $ do
    let msgContents = msgBody msg
        key         = envRoutingKey env
    result :: Either SomeException a <- try (callback key (msgContents, msg))
    case result of
      Left  err -> putStr "err in addRawConsumer: " >> print err     -- Don't do anything with error
      Right _   -> return ()
    ackEnv env

retryAMQPCallback
  :: String
  -> AMQPCallback
  -> AMQPPublisher
  -> ParentMessageID
  -> TgthrMessage
  -> RetryStatus
  -> IO CallbackResults
retryAMQPCallback service = retryAMQPCallbackManualTimeout service 10000000

retryAMQPCallbackManualTimeout
  :: HasCallStack
  => String
  -> Int
  -> AMQPCallback
  -> AMQPPublisher
  -> ParentMessageID
  -> TgthrMessage
  -> RetryStatus
  -> IO CallbackResults
retryAMQPCallbackManualTimeout service timeoutVal f pub pMid msg r = do
  didTimeout :: Either SomeException (Maybe CallbackResults) <-
    try . timeout timeoutVal $ f pub pMid msg
  case didTimeout of
    Right (Just x) -> return x
    Right Nothing ->
      putStr (service <> " Error: Timeout hit")
        >> print (toKey (tgthrBody msg), tgthrMsgid msg)
        >> error "Timeout hit"
    Left e -> do
      putStr (service <> " retry: ")
        >> print (toKey (tgthrBody msg), tgthrMsgid msg, r, e)
      throwIO e
