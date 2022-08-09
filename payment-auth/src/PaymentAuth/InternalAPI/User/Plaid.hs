{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PaymentAuth.InternalAPI.User.Plaid where

import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.ByteString.Lazy.Char8    as C
import           PaymentAuth.App.FS.PlaidBankAccount
                                                ( AccountName(AccountName)
                                                , AccountNum(AccountNum)
                                                , BankName(BankName)
                                                , RoutingNum(RoutingNum)
                                                , createFSFromPlaid
                                                )
import           PaymentAuth.InternalAPI.ProcessEvents.ProcessRiskEvent
                                                ( processRiskEvents )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..)
                                                , QueryBankFSBody(..)
                                                )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.Plaid        ( HasPlaidDB(..) )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB )
import           PaymentAuth.Monad.Time         ( HasTime )
import           PaymentAuth.Plaid              ( extractChecking )
import           PaymentAuth.Plaid.API         as Plaid
                                                ( AuthResponse
                                                  ( AuthResponse
                                                  , authItem
                                                  )
                                                , ExchangePublicTokenBody(..)
                                                , ExchangePublicTokenResponse(..)
                                                , GetAuthDataBody(..)
                                                , InstitutionDetailsBody(..)
                                                , InstitutionDetailsResponse(..)
                                                , PlaidRoutes(..)
                                                , plaidClientM
                                                , plaidIOM
                                                )
import           Servant.API                    ( NoContent(..) )
import           Servant.Client                 ( runClientM )
import           Servant.Server                 ( ServerError(errBody)
                                                , err403
                                                , err500
                                                )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Plaid.Base       ( Item(..)
                                                , PlaidAccountId(PlaidAccountId)
                                                , PlaidEnvironment(Production)
                                                )
import           Shared.Models.User             ( RedactedText(RedactedText)
                                                , UserID
                                                )
import           Shared.TgthrMessages.PaymentAuth
                                               as Msgs
                                                ( AccountDetails(..) )
import           Shared.WebAPI.PaymentAuth.API as API
                                                ( SetPlaidAccountBody(..)
                                                , TraceContext
                                                , traceToMID
                                                )

setAccountWithPlaid
  :: ( HasPlaidDB m
     , HasAccounts m
     , HasRiskScoresDB m
     , HasTime m
     , MonadIO m
     , MonadError ServerError m
     , HasEventTracking m
     )
  => TraceContext
  -> UserID
  -> SetPlaidAccountBody
  -> m NoContent
setAccountWithPlaid trace uid SetPlaidAccountBody { API.publicToken = pToken, accountId = aId }
  = do
    env      <- getPlaidEnv
    clientId <- getPlaidClientId
    secret   <- getPlaidSecret

    tracePrint trace "setAccountWithPlaid " (uid, pToken, aId)

    let getAccesstoken = _ExchangePublicToken (plaidIOM env) clientId secret
          $ ExchangePublicTokenBody pToken

    ExchangePublicTokenResponse { Plaid.accessToken = aToken, Plaid.itemId = iId } <-
      liftIO getAccesstoken
    tracePrint trace "getAccesstoken " (uid, aToken, iId)

    let mid = traceToMID trace
    insertToken mid uid (aToken, iId, Production)

    let getAuthFn =
          _GetAuthData plaidClientM clientId secret $ GetAuthDataBody aToken
    authRes <- liftIO $ runClientM getAuthFn env
    tracePrint trace "getAuthFn " (uid, authRes)

    riskChanges <- case authRes of
      Left e -> do
        traceError trace "Error: could not get balance data " (uid, e)
        throwError err500
          { errBody = C.pack $ "could not get balance data " <> show (uid, e)
          }
      Right response@AuthResponse { authItem = Item {..} } -> do
        iRes@InstitutionDetailsResponse {..} <-
          liftIO
          . _InstitutionDetails (plaidIOM env) clientId secret
          $ InstitutionDetailsBody itemInstitutionId

        tracePrint trace "setAccountWithPlaid _InstitutionDetails " (uid, iRes)
        let accounts = extractChecking response
        let thisAccount = filter
              (\AccountDetails {..} -> acctId == PlaidAccountId aId)
              accounts

        tracePrint trace
                   "setAccountWithPlaid thisAccount "
                   (uid, thisAccount, accounts, aId)
        case thisAccount of
          [] -> do
            traceError trace
                       "Error: Could not find acount "
                       (uid, aId, accounts)
            throwError err500
              { errBody = C.pack $ "Could not find acount " <> show
                            (uid, aId, accounts)
              }
          AccountDetails {..} : _ -> do
            let queryBody = QueryBankFSBody (RedactedText acctABARouting)
                                            (RedactedText acctDDNNumber)
            alreadyUsed <- queryUsersWithBank trace queryBody
            tracePrint trace "queryUsersWithBank " (uid, queryBody)

            case alreadyUsed of
              []   -> return ()
              prev -> do
                tracePrint trace
                           "User tried to link existing account "
                           (uid, acctABARouting, acctDDNNumber, prev)
                throwError err403
                  { errBody = C.pack
                                $ show (uid, "Account already in use" :: String)
                  }

            createFSFromPlaid trace
                              uid
                              acctId
                              (BankName institutionName)
                              (AccountName acctName)
                              (RoutingNum acctABARouting)
                              (AccountNum acctDDNNumber)
                              acctType

    mapM_ processRiskEvents riskChanges

    return NoContent
