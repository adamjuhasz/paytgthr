{- HLINT ignore "Use let" -}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Auditor.DB where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Text                      ( Text )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Shared.Messages
import           Shared.TgthrMessages.Base

typeOfMessage :: TgthrMessage -> Text
typeOfMessage TgthrMessage { tgthrBody = EventV1 _ } = "event"
typeOfMessage TgthrMessage { tgthrBody = CommandV1 _ } = "command"
typeOfMessage TgthrMessage { tgthrBody = ReplyV1 (ReplySuccessV1 _) } = "reply"
typeOfMessage TgthrMessage { tgthrBody = ReplyV1 (ReplyFailureV1 _) } =
  "replyfailure"

versionOfBody :: TgthrMessage -> Text
versionOfBody TgthrMessage{} = "1.0"

instance ToRow TgthrMessage where
  toRow msg@TgthrMessage {..} =
    [ toField tgthrMsgid
    , toField tgthrTimestamp
    , toField tgthrParent
    , toField tgthrSource
    , toField (typeOfMessage msg)
    , toField (versionOfBody msg)
    , toField (toKey tgthrBody)
    , toField (toJSON tgthrBody)
    ]
instance FromRow TgthrMessage where
  fromRow = do
    tgthrMsgid     <- field
    tgthrTimestamp <- field
    tgthrParent    <- field
    tgthrSource    <- field
    body :: Value  <- field
    tgthrBody      <- return . resultForce . fromJSON $ object ["value" .= body]
    tgthrUserid    <- return Nothing
    return TgthrMessage { .. }

resultForce :: Result a -> a
resultForce (Error   e) = error $ "resultForce: " <> e
resultForce (Success x) = x

insertMessage :: TgthrMessage -> Connection -> IO ()
insertMessage msg conn = void . tryE $ execute
  conn
  "INSERT INTO tgthr.audit_log (msg_id, msg_timestamp, parent_id, source_id, msg_type, msg_version, body_key, body_contents) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
  msg
 where
  tryE :: IO a -> IO (Either SomeException a)
  tryE = try

getMessage :: MessageID -> Connection -> IO (Either String TgthrMessage)
getMessage mid conn = do
  threadDelay 1000000 -- sleep so next msg comes in
  res :: Either SomeException [TgthrMessage] <- try $ query
    conn
    "SELECT msg_id, msg_timestamp, parent_id, source_id, body_contents FROM tgthr.audit_log WHERE msg_id = ?"
    (Only mid)
  case res of
    Left  _ -> return $ Left "failed"
    Right x -> return . Right $ head x
