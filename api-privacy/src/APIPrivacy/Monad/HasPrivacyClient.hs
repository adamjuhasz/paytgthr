module APIPrivacy.Monad.HasPrivacyClient where

import           APIPrivacy.PrivacyClient       ( PrivacyAuthVal )
import           Data.Text                      ( Text )
import           Servant.Client                 ( ClientEnv
                                                , ClientError
                                                , ClientM
                                                )

class Monad m => HasPrivacyClient m where
  getPrivacyEnv:: m ClientEnv
  getPrivacyAPIKey :: m Text
  getPrivacyAuth :: m PrivacyAuthVal
  getPrivacyURLBase :: m Text
  runPrivacyCmd :: ClientM a -> m (Either ClientError a)
