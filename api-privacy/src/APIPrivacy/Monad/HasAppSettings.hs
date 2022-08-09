module APIPrivacy.Monad.HasAppSettings where

class Monad m => HasAppSettings m where
  getIsShuttingDown :: m (Maybe Bool)
