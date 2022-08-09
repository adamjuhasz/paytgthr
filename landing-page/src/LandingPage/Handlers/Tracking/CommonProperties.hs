{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE RecordWildCards, StrictData #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module LandingPage.Handlers.Tracking.CommonProperties where

import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON(..)
                                                , ToJSON
                                                , withObject
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           GHC.Generics                   ( Generic )

data DeviceInfo = DeviceInfo
  { devName            :: Text
  , devBrand           :: Text
  , devManufacturer    :: Text
  , devModelName       :: Text
  , devModelId         :: Text
  , devProductName     :: Text
  , devOsVersion       :: Text
  , devPlatform        :: Text
  , devPlatformVersion :: Text
  }
  deriving (Eq, Show, Generic, ToJSON)
instance FromJSON DeviceInfo where
  parseJSON = withObject "DeviceInfo" $ \o -> do
    devName            <- o .: "deviceName"
    devBrand           <- o .: "brand"
    devManufacturer    <- o .: "manufacturer"
    devModelName       <- o .: "modelName"
    devModelId         <- o .: "modelId"
    devProductName     <- o .: "productName"
    devOsVersion       <- o .: "osVersion"
    devPlatform        <- o .: "platform"
    devPlatformVersion <- o .: "platformVersion"

    return DeviceInfo { .. }

data CellularInfo = CellularInfo
  { calCarrier :: Text
  }
  deriving (Eq, Show, Generic, ToJSON)
instance FromJSON CellularInfo where
  parseJSON = withObject "CellularInfo" $ \o -> do
    calCarrier <- o .: "carrier"

    return CellularInfo { .. }

data UserPermissions = UserPermissions
  { perPush   :: Bool
  , perCamera :: Bool
  }
  deriving (Eq, Show, Generic, ToJSON)
instance FromJSON UserPermissions where
  parseJSON = withObject "UserPermissions" $ \o -> do
    perPush   <- o .: "push"
    perCamera <- o .: "camera"

    return UserPermissions { .. }

data AppInfo = AppInfo
  { appOpens             :: Int
  , appHasAskedForReview :: Maybe UTCTime
  , appReleaseId         :: Maybe Text
  , appVersion           :: Text
  , appBuildVersion      :: Text
  , appPushToken         :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON)
instance FromJSON AppInfo where
  parseJSON = withObject "AppInfo" $ \o -> do
    appOpens             <- o .: "opens"
    appHasAskedForReview <- o .: "askedForReview"
    appReleaseId         <- o .: "releaseId"
    appVersion           <- o .: "appVersion"
    appBuildVersion      <- o .: "buildVersion"
    appPushToken         <- o .:? "pushToken"

    return AppInfo { .. }
