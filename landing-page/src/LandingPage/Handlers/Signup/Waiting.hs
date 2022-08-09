{- HLINT ignore "Redundant <&>" -}

{-# Language OverloadedStrings #-}

module LandingPage.Handlers.Signup.Waiting where

import           Control.Concurrent             ( threadDelay )
import           Data.Aeson                     ( KeyValue((.=))
                                                , Value
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Text                      ( Text )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Handlers.Router    ( routerBase )
import           LandingPage.Templating.TemplateParams
                                                ( templateParamsForUserModel )
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( AcceptType
                                                  ( ApplicationJSON
                                                  , TextHTML
                                                  )
                                                , expectSession
                                                , getAcceptType
                                                , requireUser
                                                )
import           Network.Wai                    ( Request(vault) )
import           Shared.Amqp                    ( AMQPPublisher )
import           Shared.Models.KYC              ( KYCFailureReasons(..)
                                                , KycStatus(..)
                                                )
import           Shared.Models.User             ( ClosureReason(KYCFailed)
                                                , UserModel
                                                  ( usrAptoKYCStatus
                                                  , usrUserState
                                                  )
                                                , UserState(..)
                                                )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , finish
                                                , json
                                                , liftAndCatchIO
                                                , next
                                                , request
                                                )

renderWaitingKYC :: [(Text, Value)] -> ActionM ()
renderWaitingKYC inParams = do
  accepts <- getAcceptType
  case accepts of
    ApplicationJSON -> Scotty.json . object $ ("type", "delay.KYC") : inParams
    TextHTML        -> Scotty.next

renderKYCManual :: [KYCFailureReasons] -> [(Text, Value)] -> ActionM ()
renderKYCManual reasons inParams = do
  accepts <- getAcceptType
  let publicReasons = filter (/= IdentityTheftRisk) reasons
  let jsonReasons   = "reasons" .= publicReasons
  let kycType       = ("type", "manual.kyc")
  let jsonObj       = kycType : jsonReasons : inParams
  case accepts of
    ApplicationJSON -> Scotty.json $ object jsonObj
    TextHTML        -> Scotty.next

renderKYCFailed :: [(Text, Value)] -> ActionM ()
renderKYCFailed inParams = do
  accepts <- getAcceptType
  case accepts of
    ApplicationJSON -> Scotty.json . object $ ("type", "failed.kyc") : inParams
    TextHTML        -> Scotty.next

renderKYCProcessing :: [(Text, Value)] -> ActionM ()
renderKYCProcessing inParams = do
  accepts <- getAcceptType
  case accepts of
    ApplicationJSON ->
      Scotty.json . object $ ("type", "processing.kyc") : inParams
    TextHTML -> Scotty.next

renderWaitingPartner :: [(Text, Value)] -> ActionM ()
renderWaitingPartner inParams = do
  accepts <- getAcceptType
  case accepts of
    ApplicationJSON ->
      Scotty.json . object $ ("type", "waiting.kyc") : inParams
    TextHTML -> Scotty.next

renderCardShipped :: [(Text, Value)] -> ActionM ()
renderCardShipped inParams = do
  accepts <- getAcceptType
  case accepts of
    ApplicationJSON -> Scotty.json . object $ ("type", "mailed.kyc") : inParams
    TextHTML        -> Scotty.next

-- brittany-next-binding --columns 500
kycRenderChooser :: Maybe KycStatus -> [(Text, Value)] -> ActionM ()
kycRenderChooser Nothing                     = renderKYCManual []
kycRenderChooser (Just Passed              ) = renderKYCProcessing
kycRenderChooser (Just (Rejected         _)) = renderKYCProcessing
kycRenderChooser (Just (AutoVerifyFailed r)) = renderKYCManual r

renderWaiting :: V.Key SessionData -> AMQPPublisher -> ActionM ()
renderWaiting sKey pub = do
  user <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  liftAndCatchIO $ threadDelay 1000000 -- 1000ms

  (_, thisUser, _) <- liftAndCatchIO $ routerBase pub user
  let tParams            = templateParamsForUserModel thisUser
      renderWaitingKYC'  = renderWaitingKYC tParams
      renderKYCFailed'   = renderKYCFailed tParams
      renderCardShipped' = renderCardShipped tParams
      kycStatus          = usrAptoKYCStatus thisUser

  let renderKYCManual' = kycRenderChooser kycStatus tParams
      currUsrState     = (user, usrUserState thisUser, kycStatus)
      errorMsg = putStr "Error: Bad state in KYC: " >> print currUsrState
      badState         = liftAndCatchIO errorMsg >> renderKYCManual' >> finish

  case usrUserState thisUser of
    UserWaitingOnPII     -> renderWaitingKYC' >> finish
    UserWaitingOnKYC     -> renderWaitingKYC' >> finish
    UserKYCDelay         -> renderKYCManual' >> finish
    UserUpdatedKYCDelay  -> renderKYCManual' >> finish
    UserClosed KYCFailed -> renderKYCFailed' >> finish
    UserActive           -> renderCardShipped' >> finish
    -- Bad States
    UserCreated          -> badState
    UserUpdated          -> badState
    UserClosed _         -> badState


