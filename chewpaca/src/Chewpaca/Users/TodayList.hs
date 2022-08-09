module Chewpaca.Users.TodayList where

import           Chewpaca.DB.Users
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson
import           Data.Pool                      ( withResource )
import           Shared.Database                ( PooledDB )

signedUpLast24Hours :: (MonadIO m) => PooledDB -> m Value
signedUpLast24Hours pool = do
  users <- liftIO $ withResource pool $ getUsersInLast "1 day"
  return $ toJSON users
