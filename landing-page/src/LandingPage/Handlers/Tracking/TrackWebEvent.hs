{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE RecordWildCards, StrictData #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module LandingPage.Handlers.Tracking.TrackWebEvent where

import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON(..)
                                                , Object
                                                , object
                                                , withObject
                                                )
import           Data.Text                      ( Text )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..)
                                                , toJSONField
                                                )
import           Shared.Models.Ids              ( UserID )
import           Web.Scotty                     ( ActionM
                                                -- , header
                                                -- , headers
                                                , json
                                                , jsonData
                                                , liftAndCatchIO
                                                , rescue
                                                )

instance ToField Object where
  toField = toJSONField

data WebEvent = WebEvent
  { evtName       :: Text
  , evtProperties :: Maybe Object
  , evtUser       :: Maybe UserID
  , evtDeviceId   :: Text
  }
  deriving (Eq, Show)
instance FromJSON WebEvent where
  parseJSON = withObject "EventLogging" $ \o -> do
    evtName       <- o .: "name"
    evtProperties <- o .:? "properties"
    evtUser       <- o .: "userId"
    evtDeviceId   <- o .: "deviceId"

    return WebEvent { .. }

trackWebEvent :: ActionM ()
trackWebEvent = do
  eventM :: Maybe WebEvent <-
    (Just <$> jsonData) `rescue` (\_ -> return Nothing)

  -- heads       <- headers
  -- cfCountry   <- header "CF-IPCountry"
  -- forwardedIP <- header "X-Forwarded-For"

  liftAndCatchIO $ putStr "Web Event: " >> print eventM
  -- liftAndCatchIO $ print (heads, cfCountry, forwardedIP)

  json $ object []

