{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module APIPrivacy.ExternalAPI
  ( externalApiApp
  ) where

import           APIPrivacy.AppMonad            ( APISettings
                                                , AppWebM(unAppWebM)
                                                )
import           APIPrivacy.ExternalAPI.ASA     ( processASA )
import           APIPrivacy.ExternalAPI.Models  ( ASAMessage
                                                , ASAResponse
                                                )
import           APIPrivacy.ExternalAPI.Transaction
                                                ( procesWebhook )
import           APIPrivacy.Models.Privacy      ( Transaction )
import           APIPrivacy.Monad.HasAppSettings
                                                ( HasAppSettings(..) )
import           APIPrivacy.Monad.HasClient     ( HasClient )
import           Control.Monad.Except           ( MonadError )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import           Data.Text                      ( Text )
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic         ( AsServerT
                                                , genericServeT
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

-- type PrivacyHmacHeader = Header' '[Required , Strict] "X-Privacy-HMAC" Text

-- inline brittany config for width
-- brittany-next-binding --columns 500
data Routes route = Routes
  { _HealthRoot           :: route :- Get '[PlainText, JSON] Text
  , _HealthSys            :: route :- "sys" :> "health" :> Capture "requestor" Text :> Get '[PlainText, JSON] Text
  , _AuthorizeTransaction :: route :- Header' '[Optional , Strict] "X-Cloud-Trace-Context" TraceContext :> "v1"  :> "authorize" :> ReqBody '[JSON] ASAMessage :> Post '[JSON] ASAResponse
  , _TransactionWebhook   :: route :- Header' '[Optional , Strict] "X-Cloud-Trace-Context" TraceContext :> "v1"  :> "webhook" :> ReqBody '[JSON] Transaction :> Post '[PlainText] NoContent
  }
  deriving Generic

genServerHandler
  :: (HasClient m, MonadError ServerError m, MonadIO m, HasAppSettings m)
  => Routes (AsServerT m)
genServerHandler = Routes
  { _HealthRoot           = return "Ok"
  , _HealthSys            = \svc -> getIsShuttingDown >>= \case
    Nothing    -> return "OK"
    Just False -> return "OK"
    Just True  -> do
      liftIO $ putStrLn
        ("SIGTERM received... returning 503 External to " <> show svc)
      throwError err503
  , _AuthorizeTransaction = processASA
  , _TransactionWebhook   = procesWebhook
  }

-- no need to change below

appToHandler :: APISettings -> AppWebM m -> Handler m
appToHandler cfg app = runReaderT (unAppWebM app) cfg

externalApiApp :: APISettings -> Application
externalApiApp cfg = genericServeT (appToHandler cfg) genServerHandler
