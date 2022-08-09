{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Use second" -}
{- HLINT ignore "Use bimap" -}

{-# LANGUAGE OverloadedStrings #-}

module LandingPage.Handlers.Signup.PhoneVerifier
  ( phoneVerifier
  ) where

import           Data.Aeson                     ( object )
import           Data.Functor                   ( (<&>) )
import           Data.Text                      ( Text )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUser
                                                )
import           Network.HTTP.Types             ( status403 )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.Token            ( TokenMedium(PhoneMedium) )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(..)
                                                , asClientM
                                                , incrementTrace
                                                )
import qualified Web.Scotty                    as Scotty
import           Web.Scotty                     ( ActionM
                                                , liftAndCatchIO
                                                , request
                                                , status
                                                )

phoneVerifier :: V.Key SessionData -> ClientEnv -> ActionM ()
phoneVerifier sKey accountsEnv = do
  user <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace             <- createTrace

  phoneCode :: Text <- Scotty.param "code"

  verifyTrace       <- incrementTrace trace
  let verifyFn = _TokenVerify asClientM verifyTrace user PhoneMedium phoneCode
  isVerified <- liftAndCatchIO $ runClientM verifyFn accountsEnv
  case isVerified of
    Left e ->
      error $ "Error: phoneVerifier _TokenVerify " >> show (user, phoneCode, e)
    Right False -> status status403 >> Scotty.json (object [])
    Right True  -> do
      markTrace <- incrementTrace trace
      let markVerified = _UserVerifyPhone asClientM markTrace user
      isMarked <- liftAndCatchIO $ runClientM markVerified accountsEnv
      case isMarked of
        Left e ->
          error $ "Error: emailVerifier _UserVerifyPhone " >> show (user, e)
        Right _ -> Scotty.json $ object []
