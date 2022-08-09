{-# LANGUAGE  RecordWildCards, StrictData #-}

module LandingPage.Handlers.Tracking.TrackConsole where

import           Data.Aeson                     ( FromJSON(..)
                                                , (.:)
                                                , object
                                                , withObject
                                                )
import           Data.Text                      ( Text )
import           LandingPage.Handlers.Tracking.CommonProperties
                                                ( AppInfo
                                                , UserPermissions
                                                , CellularInfo
                                                , DeviceInfo
                                                )
import           Shared.Models.User             ( UserID )
import           Web.Scotty                     ( ActionM
                                                , json
                                                , jsonData
                                                , liftAndCatchIO
                                                , rescue
                                                )

data ConsoleLogging = ConsoleLogging
  { logLevel :: Text
  , logText :: Text
  , logUser :: Maybe UserID
  , logDevice :: Text
  , logDeviceInfo :: DeviceInfo
  , logCellularInfo :: CellularInfo
  , logUserPermissions :: UserPermissions
  , logAppInfo :: AppInfo
  } deriving (Eq, Show)
instance FromJSON ConsoleLogging where
  parseJSON = withObject "ConsoleLogging" $ \o -> do
    logText            <- o .: "text"
    logUser            <- o .: "userId"
    logDevice          <- o .: "deviceToken"
    logLevel           <- o .: "level"
    logDeviceInfo      <- o .: "device"
    logCellularInfo    <- o .: "cellular"
    logUserPermissions <- o .: "permissions"
    logAppInfo         <- o .: "app"

    return ConsoleLogging { .. }

trackConsole :: ActionM ()
trackConsole = do
  logsM :: Maybe ConsoleLogging <-
    (Just <$> jsonData) `rescue` (\_ -> return Nothing)
  case logsM of
    Nothing   -> return ()
    Just logs -> liftAndCatchIO $ print logs
  json $ object []
