
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module APIDwolla.InternalAPI where

import           APIDwolla.AppMonad             ( AppWebM(..)
                                                , InternalAPISettings(..)
                                                )
import           APIDwolla.InternalAPI.AccountCreate
                                                ( createAccount )
import           APIDwolla.InternalAPI.FsCreate ( createfundingSource )
import           APIDwolla.InternalAPI.FsRemove ( removefundingSource )
import           APIDwolla.InternalAPI.PaymentCancel
                                                ( cancelPayment )
import           APIDwolla.InternalAPI.PaymentSend
                                                ( sendPayment )
import           APIDwolla.Monad.Accounts       ( HasAccounts )
import           APIDwolla.Monad.Dwolla         ( HasDwollaClient )
import           APIDwolla.Monad.Payment        ( HasPayments )
import           Control.Concurrent             ( tryReadMVar )
import           Control.Monad.Catch            ( MonadCatch )
import           Control.Monad.Except           ( MonadError )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadIO(..)
                                                , MonadReader
                                                , asks
                                                )
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Data.Text                      ( Text )
import           Network.Wai                    ( Application )
import           Servant                        ( Handler
                                                , ServerError
                                                , err503
                                                , throwError
                                                )
import           Servant.Server.Generic         ( AsServerT
                                                , genericServeT
                                                )
import           Shared.WebAPI.ApiDwolla.API    ( Routes(..) )

genServerHandler
  :: ( HasDwollaClient m
     , HasAccounts m
     , HasPayments m
     , MonadIO m
     , MonadError ServerError m
     , MonadCatch m
     , MonadReader InternalAPISettings m
     )
  => Routes (AsServerT m)
genServerHandler = Routes { _AccountCreate       = createAccount
                          , _FundingSourceCreate = createfundingSource
                          , _FundingSourceRemove = removefundingSource
                          , _PaymentInitiate     = sendPayment
                          , _PaymentCancel       = cancelPayment
                          , _health              = healthHandler
                          }

healthHandler
  :: (MonadReader InternalAPISettings m, MonadError ServerError m, MonadIO m)
  => m Text
healthHandler = do
  mvar <- asks isShuttingDown
  res  <- liftIO $ tryReadMVar mvar
  case res of
    Nothing    -> return "APIDwolla"
    Just False -> return "APIDwolla"
    Just True  -> do
      liftIO $ putStrLn "SIGTERM received... returning 503 Internal"
      throwError err503


-- no need to change below

appToHandler :: InternalAPISettings -> AppWebM m -> Handler m
appToHandler cfg app = runReaderT (unAppWebM app) cfg

internalApiApp :: InternalAPISettings -> Application
internalApiApp cfg = genericServeT (appToHandler cfg) genServerHandler
