{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleInstances, RecordWildCards, StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Group Model
Description : Model for a group
Maintainer  : adam@example.com
Stability   : experimental
-}
module Shared.Models.Group
  ( module Shared.Models.Group
  , GroupId(..)
  ) where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , genericParseJSON
                                                , genericToJSON
                                                )
import           Data.List                      ( sort )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( UTCTime )
import           Database.PostgreSQL.Simple     ( FromRow
                                                , Query
                                                , ToRow
                                                )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..)
                                                , fromJSONField
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( Action(Escape)
                                                , ToField(..)
                                                , toJSONField
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.Stack                      ( HasCallStack )
import           Servant                        ( FromHttpApiData(..)
                                                , ToHttpApiData(..)
                                                )
import           Shared.Models.Ids              ( GroupId(..)
                                                , MessageID
                                                , UserID
                                                )
import           Shared.Utils                   ( customAesonOptions )
import           Text.Read                      ( readEither )

data GroupSplit = GroupSplit
  { splUser     :: UserID
  , splRatio    :: Rational
  , splApproved :: Bool
  }
  deriving (Show, Generic)
instance FromJSON GroupSplit where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON GroupSplit where
  toJSON = genericToJSON customAesonOptions
instance ToField GroupSplit where
  toField = toJSONField
instance FromField GroupSplit where
  fromField = fromJSONField
instance Eq GroupSplit where
  a == b = splUser a == splUser b
instance ToField [GroupSplit] where
  toField = toJSONField
instance FromField [GroupSplit] where
  fromField = fromJSONField


data GroupMember = GroupMember
  { mbrUser     :: UserID
  , mbrAccepted :: Bool
  }
  deriving (Show, Generic)
instance FromJSON GroupMember where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON GroupMember where
  toJSON = genericToJSON customAesonOptions
instance ToField GroupMember where
  toField = toJSONField
instance FromField GroupMember where
  fromField = fromJSONField
instance Eq GroupMember where
  a == b = mbrUser a == mbrUser b
instance ToField [GroupMember] where
  toField = toJSONField
instance FromField [GroupMember] where
  fromField = fromJSONField


allGroupStates :: [GroupStatus]
allGroupStates =
  [ GroupCreated
  , GroupPending
  , GroupActive
  , GroupPaused
  , GroupClosed
  , GroupExpired
  , GroupDenied
  ]

data GroupStatus
  -- | Active group, first in Ord
  = GroupActive
  -- | Group has not been accepted yet
  | GroupPending
  -- | Group has been paused
  | GroupPaused
  -- | Group created
  | GroupCreated
  -- | Group has been closed by one member
  | GroupClosed
  -- | Temporary group has expired
  | GroupExpired
  -- | Group invite was not accpeted by one member
  | GroupDenied
  deriving (Show, Read, Eq, Generic, Ord)
instance ToField GroupStatus where
  toField GroupCreated = Escape "created"
  toField GroupPending = Escape "pending"
  toField GroupActive  = Escape "active"
  toField GroupPaused  = Escape "paused"
  toField GroupClosed  = Escape "closed"
  toField GroupExpired = Escape "expired"
  toField GroupDenied  = Escape "denied"
instance FromField GroupStatus where
  fromField a dat = strToGroupState <$> fromField a dat
instance FromJSON GroupStatus where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON GroupStatus where
  toJSON = genericToJSON customAesonOptions
instance FromHttpApiData GroupStatus where
  parseUrlPiece t = case readEither (T.unpack t) of
    Right s -> Right s
    Left  s -> Left $ T.pack s
instance ToHttpApiData GroupStatus where
  toUrlPiece = T.pack . show

strToGroupState :: String -> GroupStatus
strToGroupState "created" = GroupCreated
strToGroupState "pending" = GroupPending
strToGroupState "active"  = GroupActive
strToGroupState "paused"  = GroupPaused
strToGroupState "closed"  = GroupClosed
strToGroupState "expired" = GroupExpired
strToGroupState "denied"  = GroupDenied
strToGroupState _         = error "Unknown status"

data GroupModel = GroupModel
  { grpId        :: GroupId
  , grpStatus    :: GroupStatus
  , grpStart     :: Maybe UTCTime
  , grpEnd       :: Maybe UTCTime
  , grpSplit     :: [GroupSplit]
  , grpMembers   :: [GroupMember]
  , grpRevision  :: Int
  , grpVersion   :: Text
  , grpMsgSource :: MessageID
  , grpCreatedAt :: UTCTime
  }
  deriving (Show, Generic, ToRow, FromRow)
instance FromJSON GroupModel where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON GroupModel where
  toJSON = genericToJSON customAesonOptions
instance Eq GroupModel where
  a == b =
    (grpId a == grpId b)
      && (grpRevision a == grpRevision b)
      && (grpStatus a == grpStatus b)
      && (grpStart a == grpStart b)
      && (grpEnd a == grpEnd b)
      && (grpSplit a == grpSplit b)
      && (grpMembers a == grpMembers b)

(~=) :: GroupModel -> GroupModel -> Bool
a ~= b = (grpId a == grpId b) && (grpRevision a == grpRevision b)

groupModelFields :: (Query, Query)
groupModelFields =
  ( " id, status, time_start, time_end, split, members, revision, version, msg_source, created_at "
  , " ? , ?     , ?         , ?       , ?    , ?      , ?       , ?      , ?         , ?          "
  )

verifyGroupCohesion :: HasCallStack => GroupModel -> GroupModel
verifyGroupCohesion grp@GroupModel {..}
  | Prelude.null grpSplit = error ("Error: No members of group " <> show grp)
  | sum (fmap splRatio grpSplit) /= 100 = error
    (  "Error: Sum of ratios is not 100 but "
    <> show (sum (fmap splRatio grpSplit))
    <> ", "
    <> show grp
    )
  | sort (fmap splUser grpSplit) /= sort (fmap mbrUser grpMembers) = error
    ("Error: Split / Members users not equal" <> ", " <> show grp)
  | any (\r -> r < 1 || r > 100) (fmap splRatio grpSplit) = error
    ("Error: Bad split " <> show grpSplit <> ", " <> show grp)
  | otherwise = grp

groupIsActive :: UTCTime -> GroupModel -> Bool
groupIsActive now g@GroupModel { grpStatus = GroupActive } =
  (not . groupIsExpired now) g && groupHasStarted now g
groupIsActive _ _ = False

groupIsExpired :: UTCTime -> GroupModel -> Bool
groupIsExpired _   GroupModel { grpEnd = Nothing } = False
groupIsExpired now GroupModel { grpEnd = Just t }  = t < now

groupHasStarted :: UTCTime -> GroupModel -> Bool
groupHasStarted _   GroupModel { grpStart = Nothing } = True
groupHasStarted now GroupModel { grpStart = Just t }  = t < now

groupIsPending :: UTCTime -> GroupModel -> Bool
groupIsPending now g@GroupModel { grpStatus = GroupPending } =
  (not . groupIsExpired now) g
groupIsPending now g@GroupModel { grpStatus = GroupCreated } =
  (not . groupIsExpired now) g
groupIsPending _ _ = False

groupIsNotClosed :: GroupModel -> Bool
groupIsNotClosed GroupModel { grpStatus = GroupClosed }  = False
groupIsNotClosed GroupModel { grpStatus = GroupExpired } = False
groupIsNotClosed GroupModel { grpStatus = GroupDenied }  = False
groupIsNotClosed GroupModel { grpStatus = GroupCreated } = True
groupIsNotClosed GroupModel { grpStatus = GroupActive }  = True
groupIsNotClosed GroupModel { grpStatus = GroupPaused }  = True
groupIsNotClosed GroupModel { grpStatus = GroupPending } = True

groupIsPermanent :: HasCallStack => GroupModel -> Bool
groupIsPermanent GroupModel { grpStart = Nothing, grpEnd = Nothing } = True
groupIsPermanent GroupModel { grpStart = Just _, grpEnd = Just _ }   = False
groupIsPermanent g@GroupModel { grpStart = Nothing, grpEnd = Just _ } =
  error $ "Group has an end but no start " <> show g
groupIsPermanent g@GroupModel { grpStart = Just _, grpEnd = Nothing } =
  error $ "Group has a start but no end " <> show g

