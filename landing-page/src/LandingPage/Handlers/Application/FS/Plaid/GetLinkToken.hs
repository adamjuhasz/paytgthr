{-# LANGUAGE RecordWildCards #-}

module LandingPage.Handlers.Application.FS.Plaid.GetLinkToken where

import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUser
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv )
import           Shared.WebAPI.PaymentAuth.Client
                                                ( GetPlaidLinkTokenResponse
                                                  ( GetPlaidLinkTokenResponse
                                                  , linkToken
                                                  )
                                                , Routes(_GetPlaidLinkToken)
                                                , paymentauthRoutes
                                                )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , json
                                                , liftAndCatchIO
                                                , request
                                                )

getLinkToken :: V.Key SessionData -> ClientEnv -> ActionM ()
getLinkToken sKey paymentEnv = do
  user  <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace <- createTrace

  let getToken = _GetPlaidLinkToken (paymentauthRoutes paymentEnv) trace user
  GetPlaidLinkTokenResponse {..} <- liftAndCatchIO getToken

  Scotty.json $ object ["token" .= linkToken]
