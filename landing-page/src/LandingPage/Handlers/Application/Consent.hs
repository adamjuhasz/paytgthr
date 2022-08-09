module LandingPage.Handlers.Application.Consent where

import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( AcceptType
                                                  ( ApplicationJSON
                                                  , TextHTML
                                                  )
                                                , createTrace
                                                , expectSession
                                                , getAcceptType
                                                , requireUser
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.User             ( UserState(UserWaitingOnKYC) )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(..)
                                                , asClientM
                                                )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , json
                                                , liftAndCatchIO
                                                , next
                                                , request
                                                )

reConsentHandler :: V.Key SessionData -> ClientEnv -> ActionM ()
reConsentHandler sKey env = do
  user  <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace <- createTrace

  -- set new consent date
  let acceptConsentFn = _UserAcceptConsent asClientM trace user
  acceptConsentRes <- liftAndCatchIO $ runClientM acceptConsentFn env
  case acceptConsentRes of
    Left e -> error $ "Error: consetHandler _UserAcceptConsent " <> show
      (user, e, trace)
    Right _ -> return ()

  -- run KYC on them
  let runKycFn = _UserForceState asClientM trace user UserWaitingOnKYC
  kycRes <- liftAndCatchIO $ runClientM runKycFn env
  case kycRes of
    Left e ->
      error $ "Error: consetHandler _UserForceState " <> show (user, e, trace)
    Right _ -> return ()

  accepts <- getAcceptType
  case accepts of
    ApplicationJSON -> Scotty.json $ object ["success" .= True]
    TextHTML        -> Scotty.next

