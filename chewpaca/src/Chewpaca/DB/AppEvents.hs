{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Chewpaca.DB.AppEvents where

import           Data.Aeson                     ( FromJSON
                                                , Object
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple     ( Connection
                                                , Only(..)
                                                , query
                                                )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..)
                                                , fromJSONField
                                                )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow(..)
                                                , field
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Ids              ( UserID )

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
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype CellularInfo = CellularInfo
  { calCarrier :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data UserPermissions = UserPermissions
  { perPush   :: Bool
  , perCamera :: Bool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data AppInfo = AppInfo
  { appOpens             :: Int
  , appHasAskedForReview :: Maybe UTCTime
  , appReleaseId         :: Maybe Text
  , appVersion           :: Text
  , appBuildVersion      :: Text
  , appPushToken         :: Maybe Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance FromField Object where
  fromField = fromJSONField

instance FromField DeviceInfo where
  fromField = fromJSONField

instance FromField CellularInfo where
  fromField = fromJSONField

instance FromField UserPermissions where
  fromField = fromJSONField

instance FromField AppInfo where
  fromField = fromJSONField

data EventLog = EventLog
  { evtId              :: UUID
  , evtName            :: Text
  , evtProperties      :: Maybe Object
  , evtUser            :: Maybe UserID
  , evtDevice          :: Text
  , evtDeviceInfo      :: DeviceInfo
  , evtCellularInfo    :: CellularInfo
  , evtUserPermissions :: UserPermissions
  , evtAppInfo         :: AppInfo
  , evtCreatedAt       :: UTCTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
instance FromRow EventLog where
  fromRow = do
    evtId              <- field
    evtName            <- field
    evtProperties      <- field
    evtUser            <- field
    evtDevice          <- field
    evtDeviceInfo      <- field
    evtCellularInfo    <- field
    evtUserPermissions <- field
    evtAppInfo         <- field
    evtCreatedAt       <- field

    return EventLog { .. }

getAppEventsFor :: UserID -> Connection -> IO [EventLog]
getAppEventsFor uid conn = query conn qs (Only uid)
 where
  qs = [sql| 
      SELECT id, event_name, event_properties, user_id, device_id, device_info, cellular_info, device_permissions, app_info, created_at 
      FROM tgthr.app_events 
      WHERE user_id = ? 
      ORDER BY created_at DESC |]

getAppEventsFrom :: Text -> Connection -> IO [EventLog]
getAppEventsFrom deviceid conn = query conn qs (Only deviceid)
 where
  qs = [sql| 
      SELECT id, event_name, event_properties, user_id, device_id, device_info, cellular_info, device_permissions, app_info, created_at 
      FROM tgthr.app_events 
      WHERE device_id = ? 
      ORDER BY created_at DESC |]

getEventsInLast :: Text -> Connection -> IO [EventLog]
getEventsInLast interval conn = query conn qs (Only interval)
 where
  qs = [sql| 
      SELECT id, event_name, event_properties, user_id, device_id, device_info, cellular_info, device_permissions, app_info, created_at 
      FROM tgthr.app_events 
      WHERE created_at >= now() - interval ?
      ORDER BY created_at DESC |]
