{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module LandingPage.Handlers.Application.Referral where

import           Data.Aeson                     ( KeyValue((.=))
                                                , Value(Null)
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
import           Network.HTTP.Types             ( Status(Status, statusCode)
                                                , status404
                                                , status409
                                                , status500
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , ClientError(FailureResponse)
                                                , ResponseF
                                                  ( Response
                                                  , responseStatusCode
                                                  )
                                                , runClientM
                                                )
import           Shared.Console                 ( traceError )
import           Shared.Models.Referral.ReferralCode
                                                ( ReferralCode(..) )
import           Shared.Models.Referral.ReferralProgress
                                                ( ReferralProgress(..) )
import           Shared.Models.User
import           Shared.WebAPI.AccountsFSM.API  ( Routes(..) )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( accountsClientM )
import           Shared.WebAPI.General.API
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , finish
                                                , json
                                                , liftAndCatchIO
                                                , param
                                                , request
                                                , status
                                                )


getMyReferralcode :: V.Key SessionData -> ClientEnv -> ActionM ()
getMyReferralcode sKey accountsEnv = do
  user  <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace <- createTrace
  let fn = _GetRererralCode accountsClientM trace user
  res <- liftAndCatchIO $ runClientM fn accountsEnv
  case res of
    Left ce -> do
      traceError trace "Error: _GetRererralCode " (user, ce)
      status status500 >> Scotty.json (object [])
    Right rc -> Scotty.json
      $ object ["code" .= referrerCode rc, "created" .= codeCreatedAt rc]

progressToJSON :: ClientEnv -> ReferralProgress -> ActionM Value
progressToJSON accountsEnv ReferralProgress {..} = do
  trace <- createTrace
  let fn = _UserGet accountsClientM trace
  refereeModel  <- liftAndCatchIO $ runClientM (fn referee) accountsEnv
  referrerModel <- case referrer of
    Nothing -> return Nothing
    Just ui -> do
      ref <- liftAndCatchIO $ runClientM (fn ui) accountsEnv
      case ref of
        Left ce -> do
          traceError trace "Error: _UserGet " (ui, ce)
          return Nothing
        Right um -> return $ Just um
  return $ object
    [ "percentDone" .= progressDisplay
    , "expires" .= programExpiration
    , "progress" .= progress
    , "created" .= progressCreatedAt
    , "refereeName" .= case refereeModel of
      Left  _ -> Nothing
      Right UserModel { usrFirstName = Nothing } -> Nothing
      Right UserModel { usrLastName = Nothing } -> Nothing
      Right UserModel { usrFirstName = Just fname, usrLastName = Just lname }
        -> Just $ fname <> " " <> lname
    , "referrerFname" .= case referrerModel of
      Nothing                         -> Nothing
      Just UserModel { usrFirstName } -> usrFirstName
    ]

getMyReferralProgress :: V.Key SessionData -> ClientEnv -> ActionM ()
getMyReferralProgress sKey accountsEnv = do
  user  <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace <- createTrace
  let fn = _GetReferralProgress accountsClientM trace user
  res <- liftAndCatchIO $ runClientM fn accountsEnv
  case res of
    Left ce -> do
      traceError trace "Error: _GetReferralProgress " (user, ce)
      status status500 >> Scotty.json (object [])
    Right Nothing   -> Scotty.json Null
    Right (Just rp) -> progressToJSON accountsEnv rp >>= Scotty.json

getMyRefereesProgress :: V.Key SessionData -> ClientEnv -> ActionM ()
getMyRefereesProgress sKey accountsEnv = do
  user  <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace <- createTrace
  let fn = _GetReferralLinks accountsClientM trace user
  res <- liftAndCatchIO $ runClientM fn accountsEnv
  case res of
    Left ce -> do
      traceError trace "Error: _GetReferralLinks " (user, ce)
      status status500 >> Scotty.json (object [])
    Right rps -> mapM (progressToJSON accountsEnv) rps >>= Scotty.json

useAReferralCode :: V.Key SessionData -> ClientEnv -> ActionM ()
useAReferralCode sKey accountsEnv = do
  user <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  refCode :: Text <- param "code"
  trace <- createTrace

  let fn = _UseReferralCode accountsClientM trace refCode user
  res <- liftAndCatchIO $ runClientM fn accountsEnv
  case res of
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 404 } })
      -> status status404 >> finish
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 409 } })
      -> status status409 >> finish
    Left ce -> do
      traceError trace "Error: _UseReferralCode " (user, refCode, ce)
      status status500 >> Scotty.json (object [])
    Right _ -> do
      trace' <- incrementTrace trace
      let getProg = _GetReferralProgress accountsClientM trace' user
      currProfress <- liftAndCatchIO $ runClientM getProg accountsEnv
      case currProfress of
        Left ce -> do
          traceError trace "Error: _GetReferralProgress " (user, ce)
          Scotty.json Null
        Right Nothing -> do
          traceError trace "Error: _GetReferralProgress got Nothing " user
          Scotty.json Null
        Right (Just p) -> progressToJSON accountsEnv p >>= Scotty.json
      Scotty.json $ object []
