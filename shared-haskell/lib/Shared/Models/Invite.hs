{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Shared.Models.Invite where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..)
                                                , fromJSONField
                                                )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow(..)
                                                , field
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..)
                                                , toJSONField
                                                )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow(..) )
import qualified Database.PostgreSQL.Simple.Types
                                               as PT
import           GHC.Generics                   ( Generic )
import           Shared.Models.Ids              ( GroupId
                                                , UserID
                                                )

data PreFilledInfo = PreFilledInfo
  { partnerFirstName      :: Maybe Text
  , partnerLastName       :: Maybe Text
  , partnerAddressStreet  :: Maybe Text
  , partnerAddressStreet2 :: Maybe Text
  , partnerAddressCity    :: Maybe Text
  , partnerAddressState   :: Maybe Text
  , partnerAddressZip     :: Maybe Text
  , partnerDOB            :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

emptyPreFilled :: PreFilledInfo
emptyPreFilled = PreFilledInfo { partnerFirstName      = Nothing
                               , partnerLastName       = Nothing
                               , partnerAddressStreet  = Nothing
                               , partnerAddressStreet2 = Nothing
                               , partnerAddressCity    = Nothing
                               , partnerAddressState   = Nothing
                               , partnerAddressZip     = Nothing
                               , partnerDOB            = Nothing
                               }

instance ToField PreFilledInfo where
  toField = toJSONField
instance FromField PreFilledInfo where
  fromField = fromJSONField

data InviteStatus
  = Created
  | Accepted
  | Cancelled
  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)
instance ToField InviteStatus where
  toField s = toField $ show s
instance FromField InviteStatus where
  fromField a dat = read <$> fromField a dat

type InviteCode = Text

data PartnerInvite = PartnerInvite
  { inviter          :: UserID
  , groupCreated     :: Maybe GroupId
  , inviteCode       :: InviteCode
  , inviteePreFilled :: PreFilledInfo
  , inviteStatus     :: InviteStatus
  , inviteCreated    :: UTCTime
  , inviteUpdated    :: UTCTime
  , inviteRevision   :: Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToRow PartnerInvite where
  toRow PartnerInvite {..} =
    [ toField inviteCode
    , toField inviter
    , toField groupCreated
    , toField inviteePreFilled
    , toField inviteStatus
    , toField inviteCreated
    , toField inviteUpdated
    , toField inviteRevision
    ]

instance FromRow PartnerInvite where
  fromRow = do
    inviteCode       <- field
    inviter          <- field
    groupCreated     <- field
    inviteePreFilled <- field
    inviteStatus     <- field
    inviteCreated    <- field
    inviteUpdated    <- field
    inviteRevision   <- field

    return PartnerInvite { .. }

inviteFields :: (PT.Query, PT.Query)
inviteFields =
  ( " code, inviter_id, group_id, prefilled, status, created_at, updated_at, revision "
  , " ?   , ?         , ?       , ?        , ?     , ?         , ?         , ?        "
  )
