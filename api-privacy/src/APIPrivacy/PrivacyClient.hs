{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module APIPrivacy.PrivacyClient
  ( module APIPrivacy.PrivacyClient
  , CardType(..)
  , CardState(..)
  , PrivacyCard(..)
  , AccountSpendLimits(..)
  , Transaction(..)
  , TransactionToken(..)
  , mkClientEnv
  , parseBaseUrl
  , runClientM
  ) where

import           APIPrivacy.Models.Privacy      ( AccountSpendLimits(..)
                                                , CardState(..)
                                                , CardType(..)
                                                , FundingAccountToken
                                                , PrivacyCard(..)
                                                , Transaction(..)
                                                , TransactionToken(..)
                                                , currencyToCents
                                                )
import           Control.Arrow                  ( left )
import           Data.Aeson                     ( (.:)
                                                , (<?>)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Aeson.Types               ( JSONPathElement(Key) )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Maybe                     ( isJust )
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TextS
import           Data.Time.Clock                ( UTCTime )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import           Network.HTTP.Client            ( Manager
                                                , ManagerSettings(..)
                                                , Request(requestHeaders)
                                                )
import           Network.HTTP.Client.TLS        ( newTlsManagerWith
                                                , tlsManagerSettings
                                                )
import           Network.HTTP.Types             ( HeaderName )
import           Servant
import           Servant.API.Generic
import           Servant.Client                 ( ClientM
                                                , mkClientEnv
                                                , parseBaseUrl
                                                , runClientM
                                                )
import           Servant.Client.Generic         ( AsClientT
                                                , genericClient
                                                )
import           Shared.Models.Card             ( PrivacyCardToken(..) )
import           Shared.Models.Cardholder       ( PrivacyAccountToken(..) )
import           Shared.Models.Currency         ( Currency )
import           Shared.Models.User             ( EmailAddress
                                                , PhoneNumber
                                                , RedactedText
                                                )

newtype PrivacyAuthVal = PrivacyAuthVal Text deriving (Eq, Show)
instance ToHttpApiData PrivacyAuthVal where
  toUrlPiece (PrivacyAuthVal t) = toUrlPiece t

data ShippingAddress = ShippingAddress
  { shippingFirstName :: Text
  , shippingLastName  :: Text
  , shippingAddress1  :: Text
  , shippingAddress2  :: Text
  , shippingCity      :: Text
  , shippingState     :: Text
  , shippingZipcode   :: Text
  }
  deriving (Eq, Show)
instance ToJSON ShippingAddress where
  toJSON ShippingAddress {..} = object
    [ "first_name" .= shippingFirstName
    , "last_name" .= shippingLastName
    , "address1" .= shippingAddress1
    , "address2" .= shippingAddress2
    , "city" .= shippingCity
    , "state" .= shippingState
    , "zipcode" .= shippingZipcode
    , ("country", "USA")
    ]


type PrivacyAuth = Header' '[Required , Strict] "Authorization" PrivacyAuthVal
type AccountTokenQP
  = QueryParam' '[Required , Strict] "account_token" PrivacyAccountToken
data CreateCardBody = CreateCardBody
  { cardType        :: CardType
  , cardMemo        :: Maybe Text
  , cardState       :: CardState
  , spendLimit      :: Maybe Currency
  , shippingAddress :: ShippingAddress
  }
  deriving (Eq, Show)
instance ToJSON CreateCardBody where
  toJSON CreateCardBody {..} =
    object
      $  ["type" .= cardType, "state" .= cardState]
      <> [("card_a", "00000000-0000-0000-0000-000000000000")] -- Special PT card art
      <> [ ("product_id", "1") | cardType == PhysiscalCard ]
      <> [ "shipping_address" .= shippingAddress | cardType == PhysiscalCard ]
      <> [ "memo" .= cardMemo | isJust cardMemo ]
      <> case spendLimit of
           Nothing  -> []
           Just amt -> ["spend_limit" .= currencyToCents amt]

data UpdateCardBody = UpdateCardBody
  { cardToken        :: PrivacyCardToken
  , cardMemo         :: Maybe Text
  , cardState        :: Maybe CardState
  , cardFundingToken :: Maybe FundingAccountToken
  , cardPin          :: Maybe Text
  }
  deriving (Eq, Show)
instance ToJSON UpdateCardBody where
  toJSON UpdateCardBody {..} =
    object
      $  ["card_a" .= cardToken]
      <> [ "state" .= cardState | isJust cardState ]
      <> [ "funding_token" .= cardFundingToken | isJust cardFundingToken ]
      <> [ "memo" .= cardMemo | isJust cardMemo ]
      <> [ "pin" .= cardPin | isJust cardPin ]

data EnrollUserBody = EnrollUserBody
  { firstName   :: Text
  , lastName    :: Text
  , dob         :: UTCTime
  , street1     :: Text
  , street2     :: Maybe Text
  , zipCode     :: Text
  , ssnLastFour :: RedactedText
  , phoneNumber :: PhoneNumber
  , email       :: EmailAddress
  }
  deriving (Eq, Show)
instance ToJSON EnrollUserBody where
  toJSON EnrollUserBody {..} =
    object
      $  [ "first_name" .= firstName
         , "last_name" .= lastName
         , "dob" .= formatTime defaultTimeLocale "%FT%T%QZ" dob
         , "street1" .= street1
         , "zipcode" .= zipCode
         , "ssn_last_four" .= ssnLastFour
         , "phone_number" .= phoneNumber
         , "email" .= email
         ]
      <> [ "street2" .= street2 | isJust street2 ]

data PageinationResponse m = PageinationResponse
  { pagedData    :: [m]
  , page         :: Int
  , totalEntries :: Int
  , totalPages   :: Int
  }
  deriving (Eq, Show)
instance (FromJSON m) => FromJSON (PageinationResponse m) where
  parseJSON = withObject "PageinationResponse" $ \o -> do
    pagedData    <- o .: "data"
    page         <- o .: "page"
    totalEntries <- o .: "total_entries"
    totalPages   <- o .: "total_pages"

    return PageinationResponse { .. }

newtype EnrollUserResponse = EnrollUserResponse
  { accountToken :: PrivacyAccountToken
  }
  deriving (Eq, Show)
instance FromJSON EnrollUserResponse where
  parseJSON = withObject "EnrollUserResponse" $ \o -> do
    container    <- o .: "data"
    accountToken <- container .: "account_token" <?> Key "data"
    return EnrollUserResponse { .. }

data SandboxAuthorization = SandboxAuthorization
  { descriptor :: Text
  , pan        :: RedactedText
  , amount     :: Currency
  }
  deriving (Eq, Show)
instance ToJSON SandboxAuthorization where
  toJSON SandboxAuthorization {..} = object
    [ "descriptor" .= descriptor
    , "pan" .= pan
    , "amount" .= currencyToCents amount
    ]

newtype SandboxAuthorizationResponse = SandboxAuthorizationResponse
  { token :: TransactionToken
  }
  deriving (Eq, Show)
instance FromJSON SandboxAuthorizationResponse where
  parseJSON = withObject "SandboxAuthorizationResponse" $ \o -> do
    token <- o .: "token"
    return SandboxAuthorizationResponse { .. }

data SandboxVoidClearing = SandboxVoidClearing
  { token  :: TransactionToken
  , amount :: Currency
  }
  deriving (Eq, Show)
instance ToJSON SandboxVoidClearing where
  toJSON SandboxVoidClearing {..} =
    object ["token" .= token, "amount" .= currencyToCents amount]

newtype DebugResponse = DebugResponse
  { requestId :: Text
  }
  deriving (Eq, Show)
instance FromJSON DebugResponse where
  parseJSON = withObject "DebugResponse" $ \o -> do
    requestId <- o .: "debugging_request_id"
    return DebugResponse { .. }

data TextHtml

instance Accept TextHtml where
  contentType _ = "text/html"

instance MimeUnrender TextHtml Text where
  mimeUnrender _ = left show . TextS.decodeUtf8' . toStrict

-- inline brittany config for width
-- brittany-next-binding --columns 500
data Routes route = Routes
  { _EnrollUser              :: route :- PrivacyAuth :> "v1" :> "enroll" :> "consumer" :> ReqBody '[JSON] EnrollUserBody :> Post '[JSON] EnrollUserResponse
  , _GetAccount              :: route :- PrivacyAuth :> "v1" :> "account" :> AccountTokenQP :> Get '[JSON] (PageinationResponse AccountSpendLimits)
  , _CreateCard              :: route :- PrivacyAuth :> "v1" :> "card" :> AccountTokenQP :> ReqBody '[JSON] CreateCardBody :> Post '[JSON] PrivacyCard
  , _UpdateCard              :: route :- PrivacyAuth :> "v1" :> "card" :> AccountTokenQP :> ReqBody '[JSON] UpdateCardBody :> Put '[JSON] PrivacyCard
  , _ListCard                :: route :- PrivacyAuth :> "v1" :> "card" :> AccountTokenQP :> Get '[JSON] (PageinationResponse PrivacyCard)
  , _ListTransactions        :: route :- PrivacyAuth :> "v1" :> "transaction" :> "all" :> AccountTokenQP :> Get '[JSON] (PageinationResponse Transaction)
  , _PCIUI                   :: route :- "v1" :> "embed" :> "card" :> QueryParam' '[Required] "embed_request" Text :> QueryParam' '[Required] "hmac" Text :> Get '[TextHtml] Text
  , _SandboxSimAuthorization :: route :- PrivacyAuth :> "v1" :> "simulate" :> "authorize" :> ReqBody '[JSON] SandboxAuthorization :> Post '[JSON] SandboxAuthorizationResponse
  , _SandboxSimVoid          :: route :- PrivacyAuth :> "v1" :> "simulate" :> "void" :> ReqBody '[JSON] SandboxVoidClearing :> Post '[JSON] DebugResponse
  , _SandboxSimClearing      :: route :- PrivacyAuth :> "v1" :> "simulate" :> "clearing" :> ReqBody '[JSON] SandboxVoidClearing :> Post '[JSON] DebugResponse
  , _SandboxSimReturn        :: route :- PrivacyAuth :> "v1" :> "simulate" :> "return" :> ReqBody '[JSON] SandboxAuthorization :> Post '[JSON] SandboxAuthorizationResponse
  }
  deriving Generic

-- privacyRoutes :: ClientEnv -> Routes (AsClientT IO)
-- privacyRoutes env =
--   genericClientHoist (\x -> runClientM x env >>= either throwIO return)

asClientM :: Routes (AsClientT ClientM)
asClientM = genericClient

generatePrivAuth :: Text -> PrivacyAuthVal
generatePrivAuth key = PrivacyAuthVal $ "api-key " <> key

replaceHeaders :: (Eq a, IsString a) => (HeaderName, a) -> (HeaderName, a)
replaceHeaders ("Accept", "application/json;charset=utf-8,application/json") =
  ("Accept", "application/json")
replaceHeaders ("Content-Type", "application/json;charset=utf-8") =
  ("Content-Type", "application/json")
replaceHeaders (a, b) = (a, b)

newPrivacyManager :: IO Manager
newPrivacyManager = newTlsManagerWith $ tlsManagerSettings
  { managerModifyRequest = \req -> do
    let req' =
          req { requestHeaders = fmap replaceHeaders (requestHeaders req) }
    return req'
  }
