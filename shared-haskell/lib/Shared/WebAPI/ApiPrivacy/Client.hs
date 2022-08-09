{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}

module Shared.WebAPI.ApiPrivacy.Client
  ( module Shared.WebAPI.ApiPrivacy.Client
  , TraceContext(..)
  , module Shared.WebAPI.ApiPrivacy.API
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
import           Shared.WebAPI.ApiPrivacy.API

pricacyClientM :: Routes (AsClientT ClientM)
pricacyClientM = genericClient

privacyIO :: ClientEnv -> Routes (AsClientT IO)
privacyIO env =
  genericClientHoist (\x -> runClientM x env >>= either throwIO return)
