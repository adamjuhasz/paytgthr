{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE RecordWildCards, DuplicateRecordFields #-}

module PaymentAuth.Plaid.API
  ( plaidClientM
  , plaidIOM
  , generateClientEnv
  , CreateLinkTokenBody(..)
  , CreateLinkTokenResponse(..)
  , GetLinkTokenBody(..)
  , GetLinkTokenResponse(..)
  , ExchangePublicTokenBody(..)
  , ExchangePublicTokenResponse(..)
  , PublicToken(..)
  , GetAuthDataBody(..)
  , AccessToken(..)
  , ItemId(..)
  , AuthResponse(..)
  , PlaidRoutes(..)
  , InstitutionDetailsBody(..)
  , InstitutionDetailsResponse(..)
  ) where

import           Control.Exception              ( throwIO )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Network.HTTP.Client.TLS        ( newTlsManager )
import           Servant
import           Servant.API.Generic            ( Generic
                                                , GenericMode(..)
                                                )
import           Servant.Client                 ( ClientEnv
                                                , ClientM
                                                , mkClientEnv
                                                , parseBaseUrl
                                                , runClientM
                                                )
import           Servant.Client.Generic         ( AsClientT
                                                , genericClient
                                                , genericClientHoist
                                                )
import           Shared.Models.Plaid.Base       ( AccessToken(..)
                                                , AuthResponse(..)
                                                , ItemId(..)
                                                , PublicToken(..)
                                                )
import           Shared.Models.User             ( UserID(..) )

type ClientIDHeader = Header' '[Required , Strict] "PLAID-CLIENT-ID" Text
type PlaidSecretHeader = Header' '[Required , Strict] "PLAID-SECRET" Text

newtype CreateLinkTokenBody = CreateLinkTokenBody
  { userID :: UserID
  }
  deriving (Eq, Show)
instance ToJSON CreateLinkTokenBody where
  toJSON CreateLinkTokenBody {..} = object
    [ ("client_name", "Pay Tgthr")
    , ("language"   , "en")
    , "country_codes" .= ["US" :: Text]
    , "products" .= ["auth" :: Text]
    , "user" .= object ["client_user_id" .= userID]
    , "account_filters" .= object
      [ "depository"
          .= object ["account_subtypes" .= ["checking" :: Text, "savings"]]
      ]
    ]

data CreateLinkTokenResponse = CreateLinkTokenResponse
  { linkToken    :: Text
  , expiration   :: UTCTime
  , cltRequestId :: Text
  }
  deriving (Eq, Show)
instance FromJSON CreateLinkTokenResponse where
  parseJSON = withObject "CreateLinkTokenResponse" $ \o -> do
    linkToken    <- o .: "link_token"
    expiration   <- o .: "expiration"
    cltRequestId <- o .: "request_id"

    return CreateLinkTokenResponse { .. }

newtype GetLinkTokenBody = GetLinkTokenBody
  { gltLinkToken :: Text
  }
  deriving (Eq, Show)
instance ToJSON GetLinkTokenBody where
  toJSON GetLinkTokenBody {..} = object ["link_token" .= gltLinkToken]

data GetLinkTokenResponse = GetLinkTokenResponse
  { createdAt    :: UTCTime
  , webhook      :: Text
  , gltRequestId :: Text
  }
  deriving (Eq, Show)
instance FromJSON GetLinkTokenResponse where
  parseJSON = withObject "GetLinkTokenResponse" $ \o -> do
    createdAt    <- o .: "created_at"
    gltRequestId <- o .: "request_id"

    metadata     <- o .: "metadata"
    webhook      <- metadata .: "webhook"

    return GetLinkTokenResponse { .. }

newtype ExchangePublicTokenBody = ExchangePublicTokenBody
  { publicToken :: PublicToken
  } deriving (Eq, Show)
instance ToJSON ExchangePublicTokenBody where
  toJSON ExchangePublicTokenBody {..} = object ["public_token" .= publicToken]

data ExchangePublicTokenResponse = ExchangePublicTokenResponse
  { accessToken  :: AccessToken
  , itemId       :: ItemId
  , eptRequestId :: Text
  }
  deriving (Eq, Show)
instance FromJSON ExchangePublicTokenResponse where
  parseJSON = withObject "ExchangePublicTokenResponse" $ \o -> do
    accessToken  <- o .: "access_token"
    itemId       <- o .: "item_id"
    eptRequestId <- o .: "request_id"
    return ExchangePublicTokenResponse { .. }

newtype GetAuthDataBody = GetAuthDataBody {
  gadAccessToken :: AccessToken
} deriving (Eq, Show)
instance ToJSON GetAuthDataBody where
  toJSON (GetAuthDataBody t) = object ["access_token" .= t]

newtype InstitutionDetailsBody = InstitutionDetailsBody
  { institutionId :: Text
  }
  deriving (Eq, Show)
instance ToJSON InstitutionDetailsBody where
  toJSON InstitutionDetailsBody {..} = object
    ["institution_id" .= institutionId, "country_codes" .= ["US" :: Text]]

data InstitutionDetailsResponse = InstitutionDetailsResponse
  { institutionName :: Text
  , products        :: [Text]
  , routingNumbers  :: Maybe [Text]
  , oauth           :: Bool
  }
  deriving (Eq, Show)
instance FromJSON InstitutionDetailsResponse where
  parseJSON = withObject "InstitutionDetailsResponse" $ \o -> do
    institution     <- o .: "institution"
    institutionName <- institution .: "name"
    products        <- institution .: "products"
    routingNumbers  <- institution .: "routing_numbers"
    oauth           <- institution .: "oauth"
    return InstitutionDetailsResponse { .. }

-- inline brittany config for width
-- brittany-next-binding --columns 500
data PlaidRoutes route = PlaidRoutes
  { _CreateLinkToken     :: route :- ClientIDHeader :> PlaidSecretHeader :> "link" :> "token" :> "create" :> ReqBody '[JSON] CreateLinkTokenBody :> Post '[JSON] CreateLinkTokenResponse
  , _GetLinkToken        :: route :- ClientIDHeader :> PlaidSecretHeader :> "link" :> "token" :> "get" :> ReqBody '[JSON] GetLinkTokenBody :> Post '[JSON] GetLinkTokenResponse
  , _ExchangePublicToken :: route :- ClientIDHeader :> PlaidSecretHeader :> "item" :> "public_token" :> "exchange" :> ReqBody '[JSON] ExchangePublicTokenBody :> Post '[JSON] ExchangePublicTokenResponse
  , _GetAuthData         :: route :- ClientIDHeader :> PlaidSecretHeader :> "auth" :> "get" :> ReqBody '[JSON] GetAuthDataBody :> Post '[JSON] AuthResponse
  , _InstitutionDetails  :: route :- ClientIDHeader :> PlaidSecretHeader :> "institutions" :> "get_by_id" :> ReqBody '[JSON] InstitutionDetailsBody :>  Post '[JSON] InstitutionDetailsResponse
  }
  deriving Generic

plaidClientM :: PlaidRoutes (AsClientT ClientM)
plaidClientM = genericClient

plaidIOM :: ClientEnv -> PlaidRoutes (AsClientT IO)
plaidIOM env =
  genericClientHoist (\x -> runClientM x env >>= either throwIO return)

generateClientEnv :: String -> IO ClientEnv
generateClientEnv baseURL = do
  manager <- newTlsManager
  mkClientEnv manager <$> parseBaseUrl baseURL
