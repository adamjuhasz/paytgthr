{-# LANGUAGE StrictData, DeriveGeneric, DeriveAnyClass, RecordWildCards #-}

module Shared.Models.Token where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , genericParseJSON
                                                , genericToJSON
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( UUID )
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
import           Database.PostgreSQL.Simple.Types
                                                ( Query )
import           GHC.Generics                   ( Generic )
import           Servant                        ( FromHttpApiData(..)
                                                , ToHttpApiData(..)
                                                )
import           Shared.Models.Base             ( Revision )
import           Shared.Models.User             ( EmailAddress
                                                , PhoneNumber
                                                , UserID
                                                )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           Shared.Utils                   ( customAesonOptions )

newtype TokenId = TokenId UUID deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToField TokenId where
  toField (TokenId anID) = toField anID
instance FromField TokenId where
  fromField a dat = TokenId <$> fromField a dat

data ExpirationTime
  = TenMinutes
  | OneHour
  | OneDay
   deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance FromHttpApiData ExpirationTime where
  parseUrlPiece t = case t of
    "TenMinutes" -> Right TenMinutes
    "OneHour"    -> Right OneHour
    "OneDay"     -> Right OneDay
    _            -> Left "Unknown ExpirationTime"
instance ToHttpApiData ExpirationTime where
  toUrlPiece expTime = case expTime of
    TenMinutes -> "TenMinutes"
    OneHour    -> "OneHour"
    OneDay     -> "OneDay"

data TokenMedium
  = EmailMedium
  | PhoneMedium
  | PushMedium
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance FromHttpApiData TokenMedium where
  parseUrlPiece t = case t of
    "email" -> Right EmailMedium
    "sms"   -> Right PhoneMedium
    "push"  -> Right PushMedium
    _       -> Left "Unknown medium"
instance ToHttpApiData TokenMedium where
  toUrlPiece medium = case medium of
    EmailMedium -> "email"
    PhoneMedium -> "sms"
    PushMedium  -> "push"

type TokenText = Text

data TokenType
  = EmailToken EmailAddress TokenText
  | PhoneToken PhoneNumber TokenText
  | PushToken TokenText
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
instance ToField TokenType where
  toField = toJSONField
instance FromField TokenType where
  fromField = fromJSONField

data UserToken = UserToken
  { tokId        :: TokenId
  , tokRevision  :: Revision
  , tokVersion   :: Text
  , tokMsgSource :: MessageID
  , tokToken     :: TokenType
  , tokUser      :: UserID
  , tokCreatedOn :: UTCTime
  , tokExpiresAt :: Maybe UTCTime
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserToken where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON UserToken where
  toJSON = genericToJSON customAesonOptions

instance FromRow UserToken where
  fromRow = do
    tokId        <- field
    tokRevision  <- field
    tokVersion   <- field
    tokMsgSource <- field
    tokToken     <- field
    tokUser      <- field
    tokCreatedOn <- field
    tokExpiresAt <- field
    _ :: Text    <- field

    return UserToken { .. }

instance ToRow UserToken where
  toRow UserToken {..} =
    [ toField tokId
    , toField tokRevision
    , toField tokVersion
    , toField tokMsgSource
    , toField tokToken
    , toField tokUser
    , toField tokCreatedOn
    , toField tokExpiresAt
    , toField $ case tokToken of
      EmailToken _ t -> t
      PhoneToken _ t -> t
      PushToken t    -> t
    ]

userTokenFields :: (Query, Query)
userTokenFields =
  ( " id, revision, version, msg_source, token, user_id, created_on, expires_at, token_code "
  , " ?, ?, ?, ?, ?, ?, ?, ?, ? "
  )
