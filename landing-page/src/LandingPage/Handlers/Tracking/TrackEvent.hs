{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE RecordWildCards, StrictData #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}

module LandingPage.Handlers.Tracking.TrackEvent where

import           Control.Monad                  ( void )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON(..)
                                                , Object
                                                , object
                                                , withObject
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Database.PostgreSQL.Simple     ( execute )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..)
                                                , toJSONField
                                                )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow(toRow) )
import           LandingPage.Handlers.Tracking.CommonProperties
                                                ( AppInfo(..)
                                                , CellularInfo(..)
                                                , DeviceInfo(..)
                                                , UserPermissions
                                                )
import           LandingPage.Types              ( DBRunner )
import           LandingPage.Utils              ( createTrace )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.Base             ( UserID )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( IdentifyCellCarrier(..)
                                                , IdentifyDeviceBody(..)
                                                , IdentifyDeviceIP(..)
                                                , Routes(..)
                                                , accountsClientM
                                                )
import           Web.Scotty                     ( ActionM
                                                , header
                                                , json
                                                , jsonData
                                                , liftAndCatchIO
                                                , rescue
                                                )

instance ToField Object where
  toField = toJSONField

instance ToField DeviceInfo where
  toField = toJSONField

instance ToField CellularInfo where
  toField = toJSONField

instance ToField UserPermissions where
  toField = toJSONField

instance ToField AppInfo where
  toField = toJSONField

data EventLogging = EventLog
  { evtName            :: Text
  , evtProperties      :: Maybe Object
  , evtUser            :: Maybe UserID
  , evtDevice          :: Text
  , evtDeviceInfo      :: DeviceInfo
  , evtCellularInfo    :: CellularInfo
  , evtUserPermissions :: UserPermissions
  , evtAppInfo         :: AppInfo
  }
  deriving (Eq, Show)
instance FromJSON EventLogging where
  parseJSON = withObject "EventLogging" $ \o -> do
    evtName            <- o .: "name"
    evtProperties      <- o .:? "properties"
    evtUser            <- o .: "userId"
    evtDevice          <- o .: "deviceToken"
    evtDeviceInfo      <- o .: "device"
    evtCellularInfo    <- o .: "cellular"
    evtUserPermissions <- o .: "permissions"
    evtAppInfo         <- o .: "app"

    return EventLog { .. }
instance ToRow EventLogging where
  toRow EventLog {..} =
    [ toField evtName
    , toField evtProperties
    , toField evtUser
    , toField evtDevice
    , toField evtDeviceInfo
    , toField evtCellularInfo
    , toField evtUserPermissions
    , toField evtAppInfo
    ]

trackEvent :: DBRunner -> ClientEnv -> ActionM ()
trackEvent withDBPool accountsEnv = do
  eventM :: Maybe EventLogging <-
    (Just <$> jsonData) `rescue` (\_ -> return Nothing)

  trace       <- createTrace
  cfCountry   <- header "CF-IPCountry"
  forwardedIP <- header "X-Forwarded-For"

  case (eventM, forwardedIP, cfCountry) of
    (Just EventLog { evtUser = Just u }, Just forwardedIps, Just countryLazy)
      -> do
        let deviceIP = head . T.splitOn ", " $ TL.toStrict forwardedIps
        let country  = TL.toStrict countryLazy
        let identifyIP =
              _UserIdentifyDeviceIP accountsClientM trace u
                $ IdentifyDeviceIP deviceIP country
        res <- liftAndCatchIO $ runClientM identifyIP accountsEnv
        case res of
          Left ce ->
            liftAndCatchIO $ putStr "Error: _UserIdentifyDeviceIP " >> print
              (eventM, ce)
          Right _ -> return ()
        return ()
    _ -> return ()

  -- Log device Id
  case eventM of
    Just EventLog { evtUser = Nothing } -> return ()
    Nothing                             -> return ()
    Just EventLog { evtUser = Just u, evtDevice, evtDeviceInfo = DeviceInfo {..} }
      -> do
        let body = IdentifyDeviceBody
              { deviceId              = evtDevice
              , deviceName            = devName
              , deviceBrand           = devBrand
              , deviceManufacturer    = devManufacturer
              , deviceModelName       = devModelName
              , deviceModelId         = devModelId
              , deviceProductName     = devProductName
              , deviceOsVersion       = devOsVersion
              , devicePlatform        = devPlatform
              , devicePlatformVersion = devPlatformVersion
              }
        let identifyDevice = _UserIdentifyDevice accountsClientM trace u body
        res <- liftAndCatchIO $ runClientM identifyDevice accountsEnv
        case res of
          Left ce ->
            liftAndCatchIO $ putStr "Error: _UserIdentifyDeviceId " >> print
              (eventM, ce)
          Right _ -> return ()

  -- Log to console
  case eventM of
    Nothing    -> return ()
    Just event -> liftAndCatchIO $ print event

  -- save to DB
  case eventM of
    Nothing -> return ()
    Just event ->
      liftAndCatchIO $ withDBPool $ \conn -> void $ execute conn qs event

  case eventM of
    Just EventLog { evtUser = Just u, evtCellularInfo = CellularInfo { calCarrier = carrier } }
      -> do
        let identifyCarrier = _UserIdentifyCarrier accountsClientM trace u
              $ IdentifyCellCarrier carrier
        res <- liftAndCatchIO $ runClientM identifyCarrier accountsEnv
        case res of
          Left ce ->
            liftAndCatchIO $ putStr "Error: _UserIdentifyCarrier " >> print
              (eventM, ce)
          Right _ -> return ()
        return ()
    _ -> return ()

  json $ object []
 where
  qs = [sql|
    INSERT INTO tgthr.app_events 
      ( event_name
      , event_properties
      , user_id
      , device_id
      , device_info
      , cellular_info
      , device_permissions
      , app_info
      ) VALUES 
      ( ?
      , ?
      , ?
      , ?
      , ?
      , ?
      , ?
      , ?
      )
    |]
