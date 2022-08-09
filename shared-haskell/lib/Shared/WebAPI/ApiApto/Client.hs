{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}

module Shared.WebAPI.ApiApto.Client
  ( module Shared.WebAPI.ApiApto.Client
  , CardCreatedResponse(..)
  , CreateCardholderAction(..)
  , CardActivateBody(..)
  , Routes(..)
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
import           Shared.WebAPI.ApiApto.API      ( CardActivateBody(..)
                                                , CardCreatedResponse(..)
                                                , CreateCardholderAction(..)
                                                , Routes(..)
                                                )

aptoClientM :: Routes (AsClientT ClientM)
aptoClientM = genericClient

aptoRoutes :: ClientEnv -> Routes (AsClientT IO)
aptoRoutes env =
  genericClientHoist (\x -> runClientM x env >>= either throwIO return)

