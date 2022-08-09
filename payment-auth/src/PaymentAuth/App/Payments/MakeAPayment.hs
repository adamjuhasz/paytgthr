{- HLINT ignore "Replace case with fromMaybe" -}

module PaymentAuth.App.Payments.MakeAPayment
  ( makeInitialVerificationPayment
  , payOutLedger
  , makePayments
  , getUsersJournal
  , VerificationPaymentSource(..)
  , CancelPaymentErrors(..)
  ) where

import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromJust )
import           Data.UUID                      ( fromText )
import           PaymentAuth.App.CreatePayments ( FromJournal(..)
                                                , ToJournal(..)
                                                , createAPayment
                                                , signedPayAmount
                                                )
import           PaymentAuth.App.Ledger.CreateJournal
                                                ( createNewJournal )
import           PaymentAuth.App.Payments.Effects
                                                ( PaymentEffects(..) )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(getUser) )
import           PaymentAuth.Monad.Ledger       ( CreateLedgerJournalBody(..)
                                                , HasLedgerDB(..)
                                                )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..)
                                                , IncludeVerificationPayments(..)
                                                )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Currency         ( Currency(Currency)
                                                , getMonetaryValue
                                                )
import           Shared.Models.Ids              ( JournalId(..) )
import           Shared.Models.Ledger.Journal   ( JournalSearch(..)
                                                , JournalType(..)
                                                , LedgerJournal(..)
                                                )
import           Shared.Models.Payment          ( PaymentMethod(..)
                                                , PaymentSubType(..)
                                                , PaymentType(..)
                                                )
import           Shared.Models.User             ( UserID
                                                , UserModel(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

data CancelPaymentErrors
  = CantFindUser UserID
  | UserHasNoVerificationAmounts UserID
  | HasNoLedger UserID
  | HasNoJournal UserID
  | HasMultipleLedger UserID
  deriving (Show)
instance Exception CancelPaymentErrors

data VerificationPaymentSource
  = UseUsersAmounts
  | UseTheseAmounts [Double]

makeInitialVerificationPayment
  :: ( HasLedgerDB m
     , HasAccounts m
     , HasRandom m
     , HasPaymentsDB m
     , HasTime m
     , MonadIO m
     )
  => TraceContext
  -> UserID
  -> VerificationPaymentSource
  -> m [PaymentEffects]
makeInitialVerificationPayment trace uid amountsFrom = do
  userM <- getUser trace uid
  user  <- case userM of
    Just u  -> return u
    Nothing -> throw $ CantFindUser uid

  let amounts = case amountsFrom of
        UseTheseAmounts amts -> amts
        UseUsersAmounts      -> case usrBankVerifedAmounts user of
          Just amts -> fmap (* (-1)) amts
          Nothing   -> throw $ UserHasNoVerificationAmounts uid

  let dblToCurr = Currency "USD" . toRational
  makePayments trace uid $ fmap ((InitialVerification, ) . dblToCurr) amounts

getUsersJournal
  :: (HasLedgerDB m, MonadIO m)
  => TraceContext
  -> JournalSearch
  -> m LedgerJournal
getUsersJournal trace search = do
  let user = case search of
        GetPayTgthr        ui -> ui
        GetPayTgthrRewards ui -> ui
        GetStashTgthr      ui -> ui
        GetSaveTgthr       ui -> ui
        GetSecurtyDeposit  ui -> ui
        GetFundingSource   ui -> ui
        GetExternalAccount    -> error "Not a user search"
        GetVirtualAccount     -> error "Not a user search"
  journalMaybe <- getLedgerJournalType trace search
  case journalMaybe of
    [ledger] -> return ledger
    []       -> do
      traceError trace "Error: User has no Journal " search
      throw $ HasNoJournal user
    ls -> do
      traceError trace "Error: User has multiple Journals " (search, ls)
      throw $ HasMultipleLedger user

makePayments
  :: (HasLedgerDB m, HasRandom m, HasPaymentsDB m, HasTime m, MonadIO m)
  => TraceContext
  -> UserID
  -> [(PaymentSubType, Currency)]
  -> m [PaymentEffects]
makePayments trace user amounts = do
  let verificationJournal =
        JournalId . fromJust . fromText $ "00000000-0000-0000-0000-000000000000"

  -- We should not need these as they should already exist
  needsFSJournal <- getLedgerJournalType trace $ GetFundingSource user
  case needsFSJournal of
    [] -> do
      newFSId <- JournalId <$> aRandomUUID
      createNewJournal
        trace
        CreateLedgerJournalBody
          { newJournalId   = newFSId
          , newJournalType = FundingSource user $ DwollaACH "Unknown"
          , newJournalName = "Emergency FS"
          , startBalance   = Currency "USD" 0
          }
    _ : _ -> return ()

  needsPTJournal <- getLedgerJournalType trace $ GetPayTgthr user
  case needsPTJournal of
    [] -> do
      newFSId <- JournalId <$> aRandomUUID
      createNewJournal
        trace
        CreateLedgerJournalBody { newJournalId   = newFSId
                                , newJournalType = PayTgthr user
                                , newJournalName = "Emergency PT"
                                , startBalance   = Currency "USD" 0
                                }
    _ : _ -> return ()

  LedgerJournal { journalId = fsJournalId } <- getUsersJournal trace
    $ GetFundingSource user
  LedgerJournal { journalId = ptJournalId } <- getUsersJournal trace
    $ GetPayTgthr user

  let payTuple (t@InitialVerification, amt) =
        ( user
        , FromJournal fsJournalId
        , ToJournal verificationJournal
        , amt
        , DebitFromUser
        , t
        )
      payTuple (t@RefundVerification, amt) =
        ( user
        , FromJournal verificationJournal
        , ToJournal fsJournalId
        , amt
        , CreditToUser
        , t
        )
      payTuple (t@NormalPayment, amt) = if amt > 0
        then
          ( user
          , FromJournal ptJournalId
          , ToJournal fsJournalId
          , amt
          , CreditToUser
          , t
          )
        else
          ( user
          , FromJournal fsJournalId
          , ToJournal ptJournalId
          , amt
          , DebitFromUser
          , t
          )

  let payTuples = fmap payTuple amounts

  now <- getCurrentTime
  let payments = fmap (createAPayment trace now) payTuples

  finalPayments <- mapM (aRandomUUID <&>) payments
  tracePrint trace "Saving payments: " (user, finalPayments)
  mapM_ (savePayment trace) finalPayments

  return $ fmap PaymentWasCreated finalPayments

payOutLedger
  :: (HasLedgerDB m, HasPaymentsDB m, MonadIO m, HasTime m, HasRandom m)
  => TraceContext
  -> UserID
  -> m [PaymentEffects]
payOutLedger trace user = do
  pendingPayments <- getPendingPaymentsOf trace user WithoutVerifcication

  LedgerJournal { journalBalance = ptBalance } <- getUsersJournal trace
    $ GetPayTgthr user

  let pendingAmount = sum $ fmap signedPayAmount pendingPayments
  let difference    = pendingAmount + ptBalance

  tracePrint trace
             "payOutLedger current state "
             (user, ptBalance, pendingAmount, difference)

  if getMonetaryValue difference /= 0
    then do
      payments <- makePayments trace user [(NormalPayment, difference)]
      tracePrint trace "payOutLedger payment of " (user, payments)
      return payments
    else do
      tracePrint trace
                 "payOutLedger payment not needed for "
                 (user, pendingPayments, ptBalance, difference)
      return []
