module AFSM.Monad.HasCognitoClient where

import           Servant.Client                 ( ClientError
                                                , ClientM
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

class Monad m => HasCognitoClient m where
  runCognitoRoute :: TraceContext -> ClientM a -> m (Either ClientError a)
