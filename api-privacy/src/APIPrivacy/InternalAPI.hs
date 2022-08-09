
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module APIPrivacy.InternalAPI
  ( internalApiApp
  ) where

import           APIPrivacy.AppMonad            ( APISettings
                                                , AppWebM(..)
                                                )
import           APIPrivacy.InternalAPI.CardClose
                                                ( cardClose )
import           APIPrivacy.InternalAPI.CardCreate
                                                ( cardCreate )
import           APIPrivacy.InternalAPI.CardHostedUI
                                                ( getCardInfo
                                                , getHostedUIURL
                                                )
import           APIPrivacy.InternalAPI.CardSetPin
                                                ( setCardPin )
import           APIPrivacy.InternalAPI.CardSetState
                                                ( setCardState )
import           APIPrivacy.InternalAPI.CardholderCreate
                                                ( createCardholder )
import           APIPrivacy.InternalAPI.Sandbox ( sandboxAuth
                                                , sandboxClearing
                                                , sandboxReturn
                                                , sandboxVoid
                                                )
import           APIPrivacy.Monad.HasAccounts   ( HasAccounts )
import           APIPrivacy.Monad.HasAppSettings
                                                ( HasAppSettings
                                                  ( getIsShuttingDown
                                                  )
                                                )
import           APIPrivacy.Monad.HasDecryption ( HasDecryption )
import           APIPrivacy.Monad.HasPrivacyClient
                                                ( HasPrivacyClient )
import           Control.Monad.Catch            ( MonadCatch )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Network.Wai                    ( Application )
import           Servant                        ( Handler
                                                , ServerError
                                                , err503
                                                )
import           Servant.Server.Generic         ( AsServerT
                                                , genericServeT
                                                )
import           Shared.WebAPI.ApiPrivacy.API   ( Routes(..) )

genServerHandler
  :: ( HasPrivacyClient m
     , HasAccounts m
     , HasDecryption m
     , MonadIO m
     , MonadError ServerError m
     , MonadCatch m
     , HasAppSettings m
     )
  => Routes (AsServerT m)
genServerHandler = Routes
  { _health           = getIsShuttingDown >>= \case
                          Nothing    -> return "OK"
                          Just False -> return "OK"
                          Just True  -> do
                            liftIO $ putStrLn "SIGTERM received... returning 503 Internal"
                            throwError err503
  , _CardClose        = cardClose
  , _CardHostedUI     = getHostedUIURL
  , _CardCreate       = cardCreate
  , _CardholderCreate = createCardholder
  , _CardPCIInfo      = getCardInfo
  , _CardChangeState  = setCardState
  , _CardChangePin    = setCardPin
  , _SandboxAuth      = sandboxAuth
  , _SandboxVoid      = sandboxVoid
  , _SandboxClear     = sandboxClearing
  , _SandboxReturn    = sandboxReturn
  }

-- no need to change below

appToHandler :: APISettings -> AppWebM m -> Handler m
appToHandler cfg app = runReaderT (unAppWebM app) cfg

internalApiApp :: APISettings -> Application
internalApiApp cfg = genericServeT (appToHandler cfg) genServerHandler
