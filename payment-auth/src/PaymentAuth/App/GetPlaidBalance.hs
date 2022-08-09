{- HLINT ignore "Use newtype instead of data" -}

module PaymentAuth.App.GetPlaidBalance where

import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.List                      ( find )
import           PaymentAuth.App.RiskManagement.UpdateWorkflow
                                                ( ExtraFacts(..)
                                                , riskWorkFlow
                                                )
import           PaymentAuth.Monad.Accounts     ( HasAccounts )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.HttpClient   ( HasHttpClient(..) )
import           PaymentAuth.Monad.Plaid        ( HasPlaidDB(..) )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB )
import           PaymentAuth.Monad.Time         ( HasTime )
import           PaymentAuth.Plaid              ( getBalance )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Currency         ( Currency(..)
                                                , getMonetaryValue
                                                )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.Plaid.Base       ( Account(..)
                                                , BalanceResponse(balAccounts)
                                                )
import           Shared.Models.RiskScore        ( RiskFact(RiskyAcctBalance) )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

data GetPlaidBalanceErrors
  = ErrorNoAccessToken
  | ErrorNoAccount
  | ErrorNoPrimary
  | ErrorCantGetBalance
  deriving (Show)

instance Exception GetPlaidBalanceErrors

getPlaidBalance
  :: ( MonadIO m
     , HasPlaidDB m
     , HasHttpClient m
     , HasRiskScoresDB m
     , HasAccounts m
     , HasTime m
     , HasEventTracking m
     )
  => TraceContext
  -> UserID
  -> m (Maybe Currency)
getPlaidBalance trace uid = do
  let mid = traceToMID trace
  request    <- getHTTPClient
  maybeToken <- getAccessToken uid
  aToken     <- case maybeToken of
    Nothing -> do
      tracePrint trace "Error: Could not grab token for " uid
      throw ErrorNoAccessToken
    Just t -> return t

  maybePrimary <- getPrimaryAccount uid
  primary      <- case maybePrimary of
    Nothing -> do
      traceError trace "Error: Could not grab account for " uid
      throw ErrorNoAccount
    Just p -> return p

  tracePrint trace "(uid, aToken, primary)" (uid, aToken, primary)

  balEither           <- getBalance request aToken
  (measuredTime, bal) <- case balEither of
    Right (mTime, b) -> return (mTime, Just b)
    Left  e          -> do
      traceError trace "Error: Could not get balance " (uid, e)
      return (0, Nothing)

  let getPrimary anAccont = find (\a -> accountId a == anAccont) . balAccounts
  let primaryInfo = bal >>= getPrimary primary

  case primaryInfo of
    Nothing -> do
      traceError trace "Error: Can't find primary in balances for " uid
      return Nothing
    Just pInfo -> do
      saveBalance mid uid (pInfo, measuredTime)
      let balance = accountCurrentBalance pInfo
      _ <- if getMonetaryValue balance > 0
        then return Nothing
        else riskWorkFlow trace uid (RiskyAcctBalance balance) NoExtraInfo
      return $ Just balance
