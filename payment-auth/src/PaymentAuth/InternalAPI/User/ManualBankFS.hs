{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PaymentAuth.InternalAPI.User.ManualBankFS where

import           Control.Monad                  ( when )
import           Control.Monad.Catch            ( MonadCatch(..) )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.ByteString.Lazy.Char8    as C
import           PaymentAuth.App.FS.AddManualBankAccount
                                                ( SetManualBankErrors
                                                , setManualBankAccount
                                                )
import           PaymentAuth.App.FS.VerifyManualBank
                                                ( verifyManualBank )
import           PaymentAuth.InternalAPI.ProcessEvents.ProcessRiskEvent
                                                ( processRiskEvents )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..)
                                                , QueryBankFSBody(..)
                                                , UserCreateFSBody(..)
                                                )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB )
import           PaymentAuth.Monad.Time         ( HasTime )
import           Servant.API                    ( NoContent(..) )
import           Servant.Server                 ( ServerError(..)
                                                , err403
                                                )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Currency         ( Currency(Currency)
                                                , roundDownUSD
                                                )
import           Shared.Models.User             ( RedactedText(..)
                                                , UserID
                                                )
import           Shared.TgthrMessages.Base      ( AccountType(Depository)
                                                , DepositoryType(Checking)
                                                )
import           Shared.WebAPI.PaymentAuth.API  ( SetManualFSBody(..)
                                                , TraceContext
                                                , VerifyFSBankBody(..)
                                                , VerifyFSBankResponse(..)
                                                )

setManualBankFS
  :: ( HasPaymentsDB m
     , HasAccounts m
     , HasRiskScoresDB m
     , HasTime m
     , MonadIO m
     , MonadError ServerError m
     , MonadCatch m
     , HasEventTracking m
     )
  => TraceContext
  -> UserID
  -> SetManualFSBody
  -> m NoContent
setManualBankFS trace uid SetManualFSBody {..} = do
  let (RedactedText routingNum) = unverifiedABARouting
  let (RedactedText accountNum) = unverifiedDDAAccount

  -- check if someone is using this bank accout already
  let queryBody = QueryBankFSBody unverifiedABARouting unverifiedDDAAccount
  alreadyUsed <- case (unverifiedABARouting, unverifiedDDAAccount) of
    ("0000", "0000") -> return [] -- allow everyone to connect corporate Chime account
    _                             -> queryUsersWithBank trace queryBody

  case alreadyUsed of
    []   -> return ()
    prev -> do
      tracePrint trace
                 "User tried to link existing account "
                 (uid, unverifiedABARouting, unverifiedDDAAccount, prev)
      throwError err403
        { errBody = C.pack $ show (uid, "Account already in use" :: String)
        }

  (amounts, riskEvts) <-
    setManualBankAccount trace uid routingNum accountNum
      `catch` (\(e :: SetManualBankErrors) ->
                throwError err403 { errBody = C.pack $ show (uid, e) }
              )

  let
    body = UserCreateFSBody
      { bankName            = unverifiedBankName
      , accountName         = unverifiedAccountName
      , achRouting          = routingNum
      , achAccount          = accountNum
      , verified            = False
      , verificationAmounts = fmap
                                (roundDownUSD . Currency "USD" . toRational)
                                amounts
      , accountType         = Depository Checking
      }

  tracePrint trace
             "setManualBankFS setBankDetails "
             (uid, body, amounts, riskEvts)

  setBankDetails trace uid body

  mapM_ processRiskEvents riskEvts

  return NoContent

verifyManualFS
  :: (HasAccounts m, MonadIO m)
  => TraceContext
  -> UserID
  -> VerifyFSBankBody
  -> m VerifyFSBankResponse
verifyManualFS trace uid VerifyFSBankBody {..} = do
  isCorrect <- verifyManualBank trace uid userEnteredAmounts

  when isCorrect (verifyCurrentFS trace uid)

  return $ VerifyFSBankResponse isCorrect
