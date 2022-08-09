{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}

module Shared.WebAPI.ApiDwolla.Client
  ( module Shared.WebAPI.ApiDwolla.Client
  , CreateDwollaAccountResponse(..)
  , CreateFSBody(..)
  , CreateFSResponse(..)
  , Routes(..)
  , TraceContext(..)
  ) where

import           Control.Exception              ( throwIO )
import           Servant.Client                 ( ClientEnv
                                                , ClientM
                                                , runClientM
                                                )
import           Servant.Client.Generic         ( AsClientT
                                                , genericClient
                                                , genericClientHoist
                                                )
import           Shared.Models.Payment          ( PaymentId )
import           Shared.Models.User             ( FundingInformation
                                                , UserID
                                                )
import           Shared.WebAPI.ApiDwolla.API    ( CreateDwollaAccountResponse(..)
                                                , CreateFSBody(..)
                                                , CreateFSResponse(..)
                                                , Routes(..)
                                                , TraceContext(..)
                                                )

dwollaClientM :: Routes (AsClientT ClientM)
dwollaClientM = genericClient

dwollaRoutes :: ClientEnv -> Routes (AsClientT IO)
dwollaRoutes env =
  genericClientHoist (\x -> runClientM x env >>= either throwIO return)

class Monad m => HasDwollaClient m where
  createDwollaAccount :: TraceContext -> UserID -> m CreateDwollaAccountResponse
  createFundingSource :: TraceContext -> UserID -> CreateFSBody -> m CreateFSResponse
  initiatePayment :: TraceContext ->  PaymentId -> m ()
  cancelPaynent :: TraceContext ->  PaymentId -> m ()
  removeFundingSource :: TraceContext -> UserID -> FundingInformation -> m ()
  pingDwolla :: m ()
