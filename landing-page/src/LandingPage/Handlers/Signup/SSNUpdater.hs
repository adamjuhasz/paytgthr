{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Use second" -}
{- HLINT ignore "Use bimap" -}

{-# LANGUAGE OverloadedStrings #-}

module LandingPage.Handlers.Signup.SSNUpdater
  ( ssnUpdater
  ) where

import           Control.Monad                  ( when )
import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import           Data.Coerce                    ( coerce )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( EncryptedSSN(..)
                                                , SessionData
                                                )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUser
                                                )
import           LandingPage.Validators         ( ssnCheck )
import           Network.HTTP.Types             ( status400
                                                , status409
                                                , status500
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.User            as U
                                                ( UserID
                                                , UserModel(..)
                                                , UserState(..)
                                                , UserTrait(..)
                                                )
import           Shared.Vault                   ( PlainText(..) )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(..)
                                                , TraceContext
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

encryptSelective
  :: ClientEnv
  -> TraceContext
  -> UserID
  -> (PlainText -> IO EncryptedSSN)
  -> Text
  -> IO (Maybe Text)
encryptSelective _ _ _ es "123120000" =
  es (PlainText "123120000") <&> coerce <&> Just
encryptSelective _ _ _ es "123120001" =
  es (PlainText "123120001") <&> coerce <&> Just
encryptSelective _ _ _ es "123120010" =
  es (PlainText "123120010") <&> coerce <&> Just
encryptSelective _ _ _ es "123120020" =
  es (PlainText "123120020") <&> coerce <&> Just
encryptSelective _ _ _ es "123120030" =
  es (PlainText "123120030") <&> coerce <&> Just
encryptSelective accountsEnv trace uid es rawSSN = do
  encrypted <- es (PlainText rawSSN) <&> coerce
  let fn = _UsersQuerySSN asClientM trace encrypted
  res <- runClientM fn accountsEnv
  case res of
    Left e ->
      error $ "Error: encryptSelective _UsersQuerySSN " <> show (uid, e)
    Right [] -> return $ Just encrypted
    Right users ->
      let filtered = filter (\u -> usrUserID u /= uid) users
      in  if null filtered then return $ Just encrypted else return Nothing

ssnUpdater
  :: (PlainText -> IO EncryptedSSN)
  -> V.Key SessionData
  -> ClientEnv
  -> ActionM ()
ssnUpdater encrypter sKey accountsEnv = do
  user <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace          <- createTrace

  newSSN :: Text <- Scotty.param "ssn"
  validatedSSN   <- case ssnCheck newSSN of
    Nothing ->
      status status400 >> Scotty.json (object ["ssn-error" .= True]) >> finish
    Just s -> return s

  getTrace <- incrementTrace trace
  let getUser = _UserGet asClientM getTrace user
  thisUserM <- liftAndCatchIO $ runClientM getUser accountsEnv

  thisUser  <- case thisUserM of
    Left e -> do
      liftAndCatchIO $ putStr "Error: ssnUpdater _UserGet" >> print (user, e)
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

  encryptedSSN <- liftAndCatchIO
    $ encryptSelective accountsEnv trace user encrypter validatedSSN

  when
    (isNothing encryptedSSN)
    (  status status400
    >> Scotty.json (object ["ssn-reuse-error" .= True])
    >> finish
    )

  let updates = UpdateUserBody [(U.EncryptedSSN, encryptedSSN)]


  trace' <- incrementTrace trace
  let fn = _UserUpdate asClientM trace' user updates
  result <- liftAndCatchIO $ runClientM fn accountsEnv

  case result of
    Left  err -> error $ "Error: ssnUpdater UpdateUser " <> show (user, err)
    Right _   -> Scotty.json $ object []
