
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module APIApto.InternalAPI where

import           APIApto.AppMonad               ( AppWebM(..)
                                                , IntAPISettings
                                                )
import           APIApto.InternalAPI.AdminSync  ( pullSync )
import           APIApto.InternalAPI.CardActivate
                                                ( activateCard )
import           APIApto.InternalAPI.CardChangePin
                                                ( chanegeCardPin )
import           APIApto.InternalAPI.CardClose  ( closeCard )
import           APIApto.InternalAPI.CardCreate ( createNewCard )
import           APIApto.InternalAPI.CardGetLastFour
                                                ( getCardLastFour )
import           APIApto.InternalAPI.CardholderCreate
                                                ( createCardholder )
import           APIApto.InternalAPI.CardholderUpdate
                                                ( updateCardholder )
import           APIApto.Monad.Accounts         ( HasAccounts )
import           APIApto.Monad.Apto             ( HasAptoClient )
import           Control.Monad.Except           ( MonadError )
import           Control.Monad.Reader           ( MonadIO )
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Network.Wai                    ( Application )
import           Servant                        ( Handler
                                                , ServerError
                                                )
import           Servant.Server.Generic         ( AsServerT
                                                , genericServeT
                                                )
import           Shared.WebAPI.ApiApto.API      ( Routes(..) )

genServerHandler
  :: (HasAccounts m, HasAptoClient m, MonadIO m, MonadError ServerError m)
  => Routes (AsServerT m)
genServerHandler = Routes { _CardActivate     = activateCard
                          , _CardChangePin    = chanegeCardPin
                          , _CardLastFour     = getCardLastFour
                          , _CardClose        = closeCard
                          , _CardCreate       = createNewCard
                          , _CardholderCreate = createCardholder
                          , _CardholderUpdate = updateCardholder
                          , _AptoAdminPull    = pullSync
                          , _health           = return "APIApto"
                          }

-- no need to change below

appToHandler :: IntAPISettings -> AppWebM m -> Handler m
appToHandler cfg app = runReaderT (unAppWebM app) cfg

internalApiApp :: IntAPISettings -> Application
internalApiApp cfg = genericServeT (appToHandler cfg) genServerHandler
