{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Chewpaca.DB.Messages where

import           GHC.Generics                   ( Generic )
import           Database.PostgreSQL.Simple
import           Data.Aeson
import           Data.UUID                      ( UUID )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Data.Maybe

data MiniMessageRow = MiniMessageRow
  { mmrMsgId :: UUID
  , mmrKey :: Text
  , mmrTimestamp :: UTCTime
  } deriving (Eq, Show, Generic, FromRow, ToJSON)

miniMessageRowColumns :: Query
miniMessageRowColumns = " msg_id, body_key, msg_timestamp "

data MessageRow = MessageRow
  { mrMsgId :: UUID
  , mrKey :: Text
  , mrTimestamp :: UTCTime
  , mrParent :: Maybe UUID
  , mrSource :: Maybe UUID
  , mrType :: Text
  , mrBody :: Value
  , mrUser :: Maybe Text
  } deriving (Eq, Show, Generic, FromRow, ToJSON)

messageRowColumns :: Query
messageRowColumns =
  " msg_id, body_key, msg_timestamp, parent_id, source_id, msg_type, body_contents, body_contents ->> 'userid' "

getLastMessages :: Integer -> Integer -> Connection -> IO [MiniMessageRow]
getLastMessages count offset conn = query
  conn
  ("SELECT"
  <> miniMessageRowColumns
  <> "FROM tgthr.audit_log WHERE (NOT body_key LIKE 'analytics.%') AND (NOT body_key LIKE 'reply.%') AND (NOT body_key = 'account.cmd.getuser') AND (NOT body_key = 'account.cmd.getusersgroups') ORDER BY msg_timestamp desc LIMIT ? OFFSET ?"
  )
  (count, offset)

getLastMessagesFiltered
  :: Query -> Integer -> Integer -> Connection -> IO [MiniMessageRow]
getLastMessagesFiltered q count offset conn = query
  conn
  (  "SELECT"
  <> miniMessageRowColumns
  <> "FROM tgthr.audit_log WHERE "
  <> q
  <> " ORDER BY msg_timestamp desc LIMIT ? OFFSET ?"
  )
  (count, offset)

getMessagesForUser
  :: UUID -> Integer -> Integer -> Connection -> IO [MessageRow]
getMessagesForUser u count offset conn = query
  conn
  ("SELECT"
  <> messageRowColumns
  <> "FROM tgthr.audit_log WHERE body_contents ->> 'userid' = ? OR body_contents ->> 'user' = ? ORDER BY msg_timestamp desc LIMIT ? OFFSET ?"
  )
  (u, u, count, offset)

getMessage :: UUID -> Connection -> IO MessageRow
getMessage mID conn = do
  rows <- query
    conn
    ("SELECT" <> messageRowColumns <> "FROM tgthr.audit_log WHERE msg_id = ?")
    (Only mID)
  if null rows
    then error $ "No rows for message " <> show mID
    else case rows of
      []      -> error "No rows found"
      (h : _) -> return h

getMessagesRelatedTo :: MessageRow -> Connection -> IO [MessageRow]
getMessagesRelatedTo msg conn = query
  conn
  ("SELECT"
  <> messageRowColumns
  <> "FROM tgthr.audit_log WHERE source_id = ? OR msg_id = ? ORDER BY msg_timestamp desc"
  )
  (defaultMsgID, defaultMsgID)
  where defaultMsgID = fromMaybe (mrMsgId msg) (mrSource msg)
