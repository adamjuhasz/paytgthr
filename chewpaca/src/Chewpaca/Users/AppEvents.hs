{-# LANGUAGE RecordWildCards #-}

module Chewpaca.Users.AppEvents where

import           Chewpaca.DB.AppEvents          ( EventLog(..)
                                                , getAppEventsFor
                                                , getAppEventsFrom
                                                , getEventsInLast
                                                )
import           Control.Monad                  ( forM )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson                     ( KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , Value
                                                , object
                                                )
import           Data.List                      ( nub
                                                , nubBy
                                                , sortOn
                                                )
import           Data.Maybe                     ( isJust )
import           Data.Ord                       ( Down(Down) )
import           Data.Pool                      ( withResource )
import           Data.Text                      ( Text )
import           Shared.Database                ( PooledDB )
import           Shared.Models.Ids              ( UserID )

getAppEventsForUser :: (MonadIO m) => UserID -> PooledDB -> m Value
getAppEventsForUser uid pool = do
  events <- liftIO $ withResource pool $ getAppEventsFor uid
  let devices = nub $ fmap evtDevice events
  deviceEvents <- liftIO $ forM devices $ \d ->
    withResource pool (getAppEventsFrom d)
  return $ toJSON $ concat deviceEvents

getAppEventsForDevice :: (MonadIO m) => Text -> PooledDB -> m Value
getAppEventsForDevice device pool = do
  events <- liftIO $ withResource pool $ getAppEventsFrom device
  return $ toJSON events

mapEvents :: (Ord a) => (EventLog -> a) -> [EventLog] -> [Value]
mapEvents f =
  fmap (\EventLog {..} -> object ["device" .= evtDevice, "user" .= evtUser])
    . sortOn (Down . evtCreatedAt)
    . nubBy (\a b -> f a == f b)
    . sortOn f

getDevicesWithAppEvents :: (MonadIO m) => PooledDB -> m Value
getDevicesWithAppEvents pool = do
  events <- liftIO $ withResource pool $ getEventsInLast "1 day"
  let devices = mapEvents evtDevice events
  return $ toJSON devices

getUsersWithAppEvents :: (MonadIO m) => PooledDB -> m Value
getUsersWithAppEvents pool = do
  events <- liftIO $ withResource pool $ getEventsInLast "1 day"
  let devices =
        mapEvents evtDevice . filter (\EventLog {..} -> isJust evtUser) $ events
  return $ toJSON devices
