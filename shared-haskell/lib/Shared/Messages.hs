{-# LANGUAGE StrictData, RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}
{- HLINT ignore "Use newtype instead of data" -}

module Shared.Messages where

import           Control.Monad                  ( when )
import           Data.Aeson                    as A
                                                ( (.:)
                                                , (.:?)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Foldable                  ( asum )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.UUID                      ( UUID )
import qualified Data.UUID.V4                  as U
import           Debug.Trace                    ( trace )
import           GHC.Stack                      ( HasCallStack )
import           Shared.Models.User             ( UserID
                                                , UserModel
                                                )
import           Shared.TgthrMessages.Accounts  ( AccountReplies(..)
                                                , AccountReplyFailues
                                                , AccountsCmd
                                                , AccountsEvent
                                                )
import           Shared.TgthrMessages.Analytics ( AnalyticsEvent )
import           Shared.TgthrMessages.Apto      ( AptoCmd
                                                , AptoEvent
                                                , AptoReplies
                                                )
import           Shared.TgthrMessages.Base      ( MessageID(..)
                                                , ThroughMQ(..)
                                                )
import           Shared.TgthrMessages.Excpetions
                                                ( Exception )
import           Shared.TgthrMessages.Mailer    ( MailerEvent )
import           Shared.TgthrMessages.PaymentAuth
                                                ( PaymentCmd
                                                , PaymentEvents
                                                , PaymentReplies
                                                )

userFromReply :: HasCallStack => MessageBody -> UserModel
userFromReply (ReplyV1 (ReplySuccessV1 (AcctReplySuccess GetUserReply { gurUserModel = model })))
  = model
userFromReply _ = error "Not a success reply"

data EventMessage
  = AccountsEvt AccountsEvent
  | AptoEvt AptoEvent
  | AnalyticsEvt AnalyticsEvent
  | ExceptionEvt Exception
  | MailerEvt MailerEvent
  | PayEvt PaymentEvents
  deriving(Show, Eq)
instance ThroughMQ EventMessage where
  toKey (AccountsEvt  e) = toKey e
  toKey (PayEvt       e) = toKey e
  toKey (AptoEvt      e) = toKey e
  toKey (AnalyticsEvt e) = toKey e
  toKey (ExceptionEvt e) = toKey e
  toKey (MailerEvt    e) = toKey e
instance ToJSON EventMessage where
  toJSON (AccountsEvt  e) = toJSON e
  toJSON (PayEvt       e) = toJSON e
  toJSON (AptoEvt      e) = toJSON e
  toJSON (AnalyticsEvt e) = toJSON e
  toJSON (ExceptionEvt e) = toJSON e
  toJSON (MailerEvt    e) = toJSON e
instance FromJSON EventMessage where
  parseJSON = withObject "EventMessage" $ \o -> asum
    [ AccountsEvt <$> o .: "value"
    , PayEvt <$> o .: "value"
    , AptoEvt <$> o .: "value"
    , AnalyticsEvt <$> o .: "value"
    , ExceptionEvt <$> o .: "value"
    , MailerEvt <$> o .: "value"
    ]

data CommandMessage
  = AcctCmd AccountsCmd
  | PayCmd PaymentCmd
  | AptoCmd AptoCmd
  deriving(Show, Eq)
instance ThroughMQ CommandMessage where
  toKey (AcctCmd c) = toKey c
  toKey (PayCmd  c) = toKey c
  toKey (AptoCmd c) = toKey c
instance ToJSON CommandMessage where
  toJSON (AcctCmd c) = toJSON c
  toJSON (PayCmd  c) = toJSON c
  toJSON (AptoCmd c) = toJSON c
instance FromJSON CommandMessage where
  parseJSON = withObject "CommandMessage" $ \o -> do
    let _x = trace ("CommandMessage: " <> show o) o
    asum
      [ AcctCmd <$> o .: "value"
      , PayCmd <$> o .: "value"
      , AptoCmd <$> o .: "value"
      ]

data ReplySuccess
  = CmdSuccess
  | AcctReplySuccess AccountReplies
  | PayReplySuccess PaymentReplies
  | AptoReplySuccess AptoReplies
  deriving(Show, Eq)
instance ThroughMQ ReplySuccess where
  toKey _ = "reply.success"
instance ToJSON ReplySuccess where
  toJSON CmdSuccess           = object ["kind" .= ("success" :: Text)]
  toJSON (AcctReplySuccess m) = toJSON m
  toJSON (PayReplySuccess  m) = toJSON m
  toJSON (AptoReplySuccess m) = toJSON m
instance FromJSON ReplySuccess where
  parseJSON = withObject "ReplySuccess" $ \o -> do
    value       <- o .: "value"
    key :: Text <- value .: "kind"
    case key of
      "success" -> return CmdSuccess
      _         -> asum
        [ AcctReplySuccess <$> o .: "value"
        , PayReplySuccess <$> o .: "value"
        , AptoReplySuccess <$> o .: "value"
        ]

data ReplyFailure
  = GenFailureText Text
  | AcctFailure AccountReplyFailues
  deriving(Show, Eq)
instance ThroughMQ ReplyFailure where
  toKey _ = "reply.failure"
instance ToJSON ReplyFailure where
  toJSON (GenFailureText t) = object ["falError" .= t]
  toJSON (AcctFailure    t) = toJSON t
instance FromJSON ReplyFailure where
  parseJSON = withObject "ReplyFailure" $ \o -> do
    value <- o .: "value"
    asum [GenFailureText <$> value .: "falError", AcctFailure <$> o .: "value"]

data ReplyMessage
  = ReplySuccessV1 ReplySuccess
  | ReplyFailureV1 ReplyFailure
  deriving (Show, Eq)
instance ThroughMQ ReplyMessage where
  toKey (ReplySuccessV1 s) = toKey s
  toKey (ReplyFailureV1 f) = toKey f
instance ToJSON ReplyMessage where
  toJSON (ReplySuccessV1 s) = toJSON s
  toJSON (ReplyFailureV1 f) = toJSON f
instance FromJSON ReplyMessage where
  parseJSON o =
    asum [ReplySuccessV1 <$> parseJSON o, ReplyFailureV1 <$> parseJSON o]

data MessageBody
  = EventV1 EventMessage
  | CommandV1 CommandMessage
  | ReplyV1 ReplyMessage
  deriving (Show, Eq)
instance ThroughMQ MessageBody where
  toKey (EventV1   e) = toKey e
  toKey (CommandV1 c) = toKey c
  toKey (ReplyV1   r) = toKey r
instance ToJSON MessageBody where
  toJSON (EventV1   e) = toJSON e
  toJSON (CommandV1 c) = toJSON c
  toJSON (ReplyV1   r) = toJSON r
instance FromJSON MessageBody where
  parseJSON o = asum
    [ EventV1 <$> parseJSON o
    , CommandV1 <$> parseJSON o
    , ReplyV1 <$> parseJSON o
    ]

data TgthrMessage = TgthrMessage
  { tgthrMsgid     :: MessageID
  , tgthrParent    :: Maybe MessageID
  , tgthrSource    :: Maybe MessageID
  , tgthrUserid    :: Maybe UserID
  , tgthrBody      :: MessageBody
  , tgthrTimestamp :: UTCTime
  }
  deriving (Show, Eq)

instance FromJSON TgthrMessage where
  parseJSON = withObject "message" $ \m -> do
    version <- m .: "version"
    when (version /= ("1.0" :: Text)) $ fail "unknown version"
    tgthrMsgid     <- m .: "id"
    tgthrParent    <- m .:? "parent"
    tgthrSource    <- m .:? "source"
    tgthrUserid    <- m .:? "userid"
    tgthrTimestamp <- m .: "timestamp"
    tgthrBody      <- m .: "body"
    return $ TgthrMessage { .. }

instance ToJSON TgthrMessage where
  toJSON TgthrMessage { tgthrBody = EventV1 e, ..} = object
    [ "id" .= tgthrMsgid
    , "parent" .= tgthrParent
    , "source" .= tgthrSource
    , "userid" .= tgthrUserid
    , "timestamp" .= tgthrTimestamp
    , "type" .= ("event" :: Text)
    , "version" .= ("1.0" :: Text)
    , "body" .= object ["name" .= toKey e, "value" .= e]
    ]
  toJSON TgthrMessage { tgthrBody = CommandV1 c, ..} = object
    [ "id" .= tgthrMsgid
    , "parent" .= tgthrParent
    , "source" .= tgthrSource
    , "userid" .= tgthrUserid
    , "timestamp" .= tgthrTimestamp
    , "type" .= ("command" :: Text)
    , "version" .= ("1.0" :: Text)
    , "body" .= object ["name" .= toKey c, "value" .= c]
    ]
  toJSON TgthrMessage { tgthrBody = ReplyV1 (ReplySuccessV1 s), ..} = object
    [ "id" .= tgthrMsgid
    , "parent" .= tgthrParent
    , "source" .= tgthrSource
    , "userid" .= tgthrUserid
    , "timestamp" .= tgthrTimestamp
    , "type" .= ("reply" :: Text)
    , "version" .= ("1.0" :: Text)
    , "body" .= object ["name" .= toKey s, "value" .= s]
    ]
  toJSON TgthrMessage { tgthrBody = ReplyV1 (ReplyFailureV1 f), ..} = object
    [ "id" .= tgthrMsgid
    , "parent" .= tgthrParent
    , "source" .= tgthrSource
    , "userid" .= tgthrUserid
    , "timestamp" .= tgthrTimestamp
    , "type" .= ("replyfailure" :: Text)
    , "version" .= ("1.0" :: Text)
    , "body" .= object ["name" .= toKey f, "value" .= f]
    ]

createTgthrMsg
  :: (UUID, UTCTime) -> Maybe TgthrMessage -> MessageBody -> TgthrMessage
createTgthrMsg (uuid, now) tgthrParentMsg msgContents = TgthrMessage
  { tgthrMsgid     = MessageID uuid
  , tgthrParent    = case tgthrParentMsg of
                       Just tgthrParentMsg' -> Just $ tgthrMsgid tgthrParentMsg'
                       Nothing              -> Nothing
  , tgthrSource    = case tgthrParentMsg of
                       Just tgthrParentMsg' -> case tgthrSource tgthrParentMsg' of
                         Just tgthrSource -> Just tgthrSource
                         Nothing          -> Just $ tgthrMsgid tgthrParentMsg'
                       Nothing -> Nothing
  , tgthrTimestamp = now
  , tgthrBody      = msgContents
  , tgthrUserid    = case tgthrParentMsg of
                       Just p  -> tgthrUserid p
                       Nothing -> Nothing
  }

defaultTgthrMsg :: MessageBody -> IO TgthrMessage
defaultTgthrMsg body = do
  uuid <- U.nextRandom
  now  <- getCurrentTime
  return $ createTgthrMsg (uuid, now) Nothing body

failureWithText :: Text -> ReplyMessage
failureWithText t = ReplyFailureV1 $ GenFailureText t

genSuccess :: ReplyMessage
genSuccess = ReplySuccessV1 CmdSuccess
