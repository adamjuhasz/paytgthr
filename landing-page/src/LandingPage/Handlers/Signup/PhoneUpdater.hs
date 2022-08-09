{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Use second" -}
{- HLINT ignore "Use bimap" -}

{-# LANGUAGE OverloadedStrings #-}

module LandingPage.Handlers.Signup.PhoneUpdater
  ( phoneUpdater
  ) where

import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Text                      ( Text )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUser
                                                )
import           LandingPage.Validators         ( phoneCheck )
import           Network.HTTP.Types             ( status400
                                                , status409
                                                , status500
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.User             ( UserModel(..)
                                                , UserState(..)
                                                , UserTrait(..)
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(..)
                                                , UpdateUserBody(..)
                                                , asClientM
                                                , incrementTrace
                                                )
import qualified Web.Scotty                    as Scotty
import           Web.Scotty                     ( ActionM
                                                , finish
                                                , liftAndCatchIO
                                                , request
                                                , status
                                                )

phoneUpdater :: V.Key SessionData -> ClientEnv -> ActionM ()
phoneUpdater sKey accountsEnv = do
  user <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace            <- createTrace

  newPhone :: Text <- Scotty.param "phone"
  validatedPhone   <- case phoneCheck newPhone of
    Nothing ->
      status status400 >> Scotty.json (object ["phone-error" .= True]) >> finish
    Just s -> return s

  getTrace <- incrementTrace trace
  let getUser = _UserGet asClientM getTrace user
  thisUserM <- liftAndCatchIO $ runClientM getUser accountsEnv

  thisUser  <- case thisUserM of
    Left e -> do
      liftAndCatchIO $ putStr "Error: phoneUpdater _UserGet" >> print (user, e)
      status status500 >> Scotty.json (object []) >> finish
    Right u -> return u

  -- Only allow updates if user is the following states
  -- UserCreated, UserWaitingOnPII, UserWaitingOnKYC
  let errorBadUserState = status status409 >> Scotty.json (object []) >> finish

  _ <- case usrUserState thisUser of
    UserCreated         -> return ()
    UserWaitingOnPII    -> return ()
    UserWaitingOnKYC    -> return ()
    UserKYCDelay        -> errorBadUserState
    UserUpdated         -> errorBadUserState
    UserUpdatedKYCDelay -> errorBadUserState
    UserActive          -> errorBadUserState
    UserClosed _        -> errorBadUserState


  let updates = UpdateUserBody [(Phone, Just validatedPhone)]

  trace' <- incrementTrace trace
  let fn = _UserUpdate asClientM trace' user updates
  result <- liftAndCatchIO $ runClientM fn accountsEnv

  case result of
    Left  err -> error $ "Error: phoneUpdater UpdateUser " <> show (user, err)
    Right _   -> Scotty.json $ object []
