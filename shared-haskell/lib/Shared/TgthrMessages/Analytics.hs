-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, StrictData #-}

module Shared.TgthrMessages.Analytics where

---------------------------------------------------------------
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( NominalDiffTime )
import           Data.Aeson                     ( genericParseJSON
                                                , genericToJSON
                                                , FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                )
import           GHC.Generics                   ( Generic )
import           Shared.Utils                   ( customAesonOptions )
import           Shared.TgthrMessages.Base      ( ThroughMQ(..) )
---------------------------------------------------------------

data AnalyticsEvent
  = WebRequestedV1
    { wreMethod :: Text
    , wrePath :: Text
    , wreUserAgent :: Text
    , wreUserid :: Text
    , wreHeaders :: [(Text, Text)]
    }
  |  APIRequestV1
    { areMethod :: Text
    , arePath :: Text
    , areService :: Text
    , areBody :: Maybe Text
    , areHeaders :: [(Text, Text)]
    }
  | ChewpacaRequestV1
    { creMethod :: Text
    , crePath :: Text
    , creUserAgent :: Text
    }
  | RequestCompleteV1
    { rceStatus :: Int
    , rceElapsedTime :: NominalDiffTime
    }
  deriving (Eq, Show, Generic)

instance FromJSON AnalyticsEvent where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON AnalyticsEvent where
  toJSON = genericToJSON customAesonOptions
instance ThroughMQ AnalyticsEvent where
  toKey WebRequestedV1{}    = "analytics.webrequest"
  toKey APIRequestV1{}      = "analytics.apirequest"
  toKey ChewpacaRequestV1{} = "analytics.chewpacarequest"
  toKey RequestCompleteV1{} = "analytics.requestcomplete"
