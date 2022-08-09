{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}

module Shared.WebAPI.PaymentAuth.Client
  ( module Shared.WebAPI.PaymentAuth.Client
  , module Shared.WebAPI.PaymentAuth.API
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
import           Shared.WebAPI.PaymentAuth.API -- Leave blank to re-export all

payAuthClientM :: Routes (AsClientT ClientM)
payAuthClientM = genericClient

paymentauthRoutes :: ClientEnv -> Routes (AsClientT IO)
paymentauthRoutes env =
  genericClientHoist (\x -> runClientM x env >>= either throwIO return)

