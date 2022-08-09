{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Use second" -}
{- HLINT ignore "Use bimap" -}

{-# LANGUAGE OverloadedStrings #-}

module LandingPage.Handlers.Signup.EmailVerifier
  ( emailVerifier
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
import           Shared.Models.Token            ( TokenMedium(EmailMedium) )
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

emailVerifier :: V.Key SessionData -> ClientEnv -> ActionM ()
emailVerifier sKey accountsEnv = do
  user <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace             <- createTrace

  emailCode :: Text <- Scotty.param "code"

  verifyTrace       <- incrementTrace trace
  let verifyFn = _TokenVerify asClientM verifyTrace user EmailMedium emailCode
  isVerified <- liftAndCatchIO $ runClientM verifyFn accountsEnv
  case isVerified of
    Left e ->
      error $ "Error: emailVerifier _TokenVerify " >> show (user, emailCode, e)
    Right False -> status status403 >> Scotty.json (object [])
    Right True  -> do
      markTrace <- incrementTrace trace
      let markVerified = _UserVerifyEmail asClientM markTrace user
      isMarked <- liftAndCatchIO $ runClientM markVerified accountsEnv
      case isMarked of
        Left e ->
          error $ "Error: emailVerifier _UserVerifyEmail " >> show (user, e)
        Right _ -> Scotty.json $ object []
