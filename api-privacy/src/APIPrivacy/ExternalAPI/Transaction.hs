{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module APIPrivacy.ExternalAPI.Transaction
  ( procesWebhook
  , processPrivacyEvent
  ) where

import           APIPrivacy.Models.Privacy     as Privacy
                                                ( EventToken(..)
                                                , EventType(..)
                                                , Merchant(..)
                                                , PrivacyCard(..)
                                                , Transaction(..)
                                                , TransactionEvent(..)
                                                , TransactionResult(..)
                                                , TransactionStatus(..)
                                                , TransactionToken(..)
                                                )
import           APIPrivacy.Monad.HasClient     ( HasClient(..) )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.Text                     as T
import           Data.UUID.V4                   ( nextRandom )
import           Servant                        ( ServerError
                                                , err404
                                                , err500
                                                )
import           Servant.API                    ( NoContent(..) )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import qualified Shared.Models.Card            as Card
import           Shared.Models.Currency         ( Currency(Currency) )
import           Shared.Models.Transaction     as Tgthr
                                                ( AptoAdjustment(..)
                                                , AptoAdjustmentID(..)
                                                , AptoAdjustmentType(..)
                                                , CardNetwork(..)
                                                , DeclineReason(..)
                                                , MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                , TransactionDetails(..)
                                                , TransactionEvent(..)
                                                , TransactionId(..)
                                                , TransactionSource(..)
                                                , TransactionState(..)
                                                )
import           Shared.Models.User             ( UserModel(usrUserID) )
import           Shared.Utils.Retry
import           Shared.WebAPI.AccountsFSM.Client
                                                ( Routes(_UsersQueryCard)
                                                , TraceContext
                                                , accountsClientM
                                                , incrementTrace
                                                , randomTrace
                                                )
import           Shared.WebAPI.PaymentAuth.Client
                                               as PayAuth
                                                ( Routes
                                                  ( _QueryPurchaseSourceId
                                                  , _UpdatePurchase
                                                  )
                                                , UpdatePurchaseBody(..)
                                                , payAuthClientM
                                                )

privacyEvent2AptoAdj :: Privacy.TransactionEvent -> AptoAdjustment
privacyEvent2AptoAdj TransactionEvent { eventToken = EventToken token, ..} =
  AptoAdjustment
    { adjId              = AptoAdjustmentID token
    , adjCreatedAt       = created
    , adjAmountLocal     = amount * (-1) -- Privacy & Tgthr have opposite signs
    , adjAmountBilling   = amount * (-1) -- Privacy & Tgthr have opposite signs
    , adjFSTransactionId = Nothing
    , adjType            = case eventType of
                             Authorization       -> AuthorizationAdjustment
                             AuthorizationAdvice -> AuthorizationAdjustment
                             Clearing            -> CaptureAdjustment
                             Void                -> CaptureAdjustment
                             Return              -> RefundAdjustment
    }

-- inline brittany config for width
-- brittany-next-binding --columns 500
keepNonAdvice :: Privacy.TransactionEvent -> Bool
keepNonAdvice TransactionEvent { eventType = Authorization, eventResult = _ } = True
keepNonAdvice TransactionEvent { eventType = AuthorizationAdvice, eventResult = _ } = True
keepNonAdvice TransactionEvent { eventType = Clearing, eventResult = Approved } = True
keepNonAdvice TransactionEvent { eventType = Void, eventResult = Approved } = False
keepNonAdvice TransactionEvent { eventType = Return, eventResult = Approved } = True
-- Filter everything else out
keepNonAdvice TransactionEvent{} = False

processPrivacyEvent :: [Privacy.TransactionEvent] -> [AptoAdjustment]
processPrivacyEvent = fmap privacyEvent2AptoAdj . filter keepNonAdvice

procesWebhook
  :: (HasClient m, MonadError ServerError m, MonadIO m)
  => Maybe TraceContext
  -> Privacy.Transaction
  -> m NoContent
procesWebhook traceM msg@Privacy.Transaction { trxToken = TransactionToken token, card = PrivacyCard { cardToken = cardToken }, merchant = Merchant {..}, ..}
  = do
    trace     <- maybe randomTrace return traceM

    cardTrace <- incrementTrace trace
    let getUser = _UsersQueryCard accountsClientM cardTrace
          $ Card.PayWithPrivacy cardToken

    userRes <- accountsClient getUser
    user    <- case userRes of
      Right (u : _) -> return u
      Right []      -> do
        traceError
          trace
          "Error: procesWebhook _UsersQueryCard Could not find a user for this card "
          msg
        throwError err404
      Left e -> do
        traceError
          trace
          "Error: procesWebhook _UsersQueryCard Could query AccountsEnv "
          (e, msg)
        throwError err500
    let purchaser = usrUserID user

    trace' <- incrementTrace trace
    let getExisting = _QueryPurchaseSourceId payAuthClientM trace' token
    existingRes <- payAuthClient getExisting
    theTrxId    <- case existingRes of
      Right (Just Tgthr.Transaction {..}) -> return trxId
      Right Nothing                       -> TransactionId <$> liftIO nextRandom
      Left  e                             -> do
        traceError trace
                   "Error: Could not find a trx for this transaction "
                   (user, e, msg)
        throwError err500

    tracePrint trace "Processsing Purchase: " (token, theTrxId, msg)

    let
      updates = UpdatePurchaseBody
        { purchaser        = purchaser
        , transactionId    = theTrxId
        , source           = PayWithPrivacy
        , idempotency      = Nothing
        , sourceId         = token
        , cardId           = Card.PayWithPrivacy cardToken
        , details          = CardTransaction { pcpContext         = "Privacy"
                                             , pcpIsCardPresent   = False
                                             , pcpIsOnline        = False
                                             , pcpIsInternational = False
                                             , pcpNetwork         = Mastercard
                                             , pcpIsEMV           = False
                                             , pcpType            = Nothing
                                             , pcpDescription = Just descriptor
                                             }
        , merchant = CardMerchant { cmiMcc = MastercardMCC $ T.pack $ show mcc
                                  , cmiMccDesc  = ""
                                  , cmiName     = descriptor
                                  , cmiLocality = Just city
                                  , cmiRegion   = Just state
                                  , cmiCountry  = country
                                  }
        , standin          = False
        , state            = stateConverter trxStatus
        , createdAt        = created
        , transactionEvent = StateTransition
        , amountLocal      = Currency "USD" 0
        , amountHold       = Currency "USD" 0
        , amountCashback   = Currency "USD" 0
        , amountFee        = Currency "USD" 0
        , amountBilling    = amount * (-1) -- Privacy & Tgthr have opposite signs
        , description      = Just descriptor
        , adjustments      = processPrivacyEvent events
        }

    trace'' <- incrementTrace trace
    let updateTrx = _UpdatePurchase payAuthClientM trace'' theTrxId updates
    updateRes <-
      retryEither
          trace
          "APIPrivacy.ExternalAPI.Transaction.procesWebhook _UpdatePurchase"
          100
        $ payAuthClient updateTrx
    case updateRes of
      Right NoContent -> return ()
      Left  e         -> do
        traceError trace
                   "Error: Could not update trx for this transaction "
                   (theTrxId, usrUserID user, e, msg, updates)
        throwError err500
    return NoContent

-- inline brittany config for width
-- brittany-next-binding --columns 500
stateConverter :: TransactionStatus -> TransactionState
stateConverter trxStatus = case trxStatus of
  PendingTrx                              -> TrxPending
  VoidedTrx                               -> TrxCompleted
  SettlingTrx                             -> TrxCompleted
  SettledTrx                              -> TrxCompleted
  BoundedTrx                              -> TrxCompleted
  DeclinedTrx CardIsPaused                -> TrxDeclined CardInactive
  DeclinedTrx CardIsClosed                -> TrxDeclined CardClosed
  DeclinedTrx GlobalTransactionLimit      -> TrxDeclined $ Unknown "GlobalTransactionLimit"
  DeclinedTrx GlobalWeeklyLimit           -> TrxDeclined $ Unknown "GlobalWeeklyLimit"
  DeclinedTrx GlobalMonthlyLimit          -> TrxDeclined $ Unknown "GlobalMonthlyLimit"
  DeclinedTrx UserTransactionLimit        -> TrxDeclined $ Unknown "UserTransactionLimit"
  DeclinedTrx UnauthorizedMerchant        -> TrxDeclined InvalidMerchant
  DeclinedTrx SingleUseRecharged          -> TrxDeclined $ Unknown "SingleUseRecharged"
  DeclinedTrx BankConectionError          -> TrxDeclined $ Unknown "BankConectionError"
  DeclinedTrx InsufficientFunds           -> TrxDeclined $ Unknown "Privacy.InsufficientFunds"
  DeclinedTrx MerchantBlacklist           -> TrxDeclined RiskyMerchant
  DeclinedTrx InvalidCardDetails          -> TrxDeclined IncorrectCVV
  DeclinedTrx BankNotVerified             -> TrxDeclined $ Unknown "BankNotVerified"
  DeclinedTrx InactiveAccount             -> TrxDeclined $ Unknown "InactiveAccount"
  DeclinedTrx AccountStateTransactionFail -> TrxDeclined $ Unknown "AccountStateTransactionFail"
  DeclinedTrx UnknownHostTimeout          -> TrxDeclined $ Unknown "UnknownHostTimeout"
  DeclinedTrx SwitchInoperativeAdvice     -> TrxDeclined $ Unknown "SwitchInoperativeAdvice"
  DeclinedTrx FraudAdvice                 -> TrxDeclined SuspectFraud
  DeclinedTrx Privacy.IncorrectPin        -> TrxDeclined Tgthr.IncorrectPin
  DeclinedTrx UnknownError                -> TrxDeclined $ Unknown "UnknownError"
  DeclinedTrx MalformedResponse           -> TrxDeclined $ Unknown "MalformedResponse"
  DeclinedTrx GeneralDecline              -> TrxDeclined $ Unknown "GeneralDecline"
  DeclinedTrx Approved                    -> error "Error: Declined does not match Approved"
