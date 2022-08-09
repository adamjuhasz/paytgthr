module APIPrivacy.Monad.HasClient where

import           Servant.Client                 ( ClientError
                                                , ClientM
                                                )

class Monad m => HasClient m where
  accountsClient :: ClientM a -> m (Either ClientError a)
  payAuthClient :: ClientM a -> m (Either ClientError a)
