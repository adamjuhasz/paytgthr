{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module App.Ledger.CreditSpec where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( nil )
import           PaymentAuth.App.Ledger.SchedulePayments
                                                ( schedulePaymentsFromLedger )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..) )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentId(..)
                                                , PaymentMethod(..)
                                                , PaymentStatus(PaymentCreated)
                                                , PaymentSubType(NormalPayment)
                                                , PaymentType(DebitFromUser)
                                                )
import           Shared.Models.Transaction      ( TransactionId(..) )
import           Shared.Models.User             ( UserID(..) )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           Shared.Utils                   ( stringToTime )
import           Shared.WebAPI.General.API      ( midToTrace )
import           Test.Hspec                     ( Spec
                                                , it
                                                , parallel
                                                , shouldBe
                                                )

user1 :: UserID
user1 = UserID nil

aMsgId :: MessageID
aMsgId = MessageID nil

aTrxId :: TransactionId
aTrxId = TransactionId nil

aPayId :: PaymentId
aPayId = PaymentId nil

aTime :: UTCTime
aTime = stringToTime "2019-10-04T20:15:32+00:00"

debitPayment :: Payment
debitPayment = Payment
  { payId          = PaymentId nil
  , payRevision    = 1
  , payVersion     = "1.0"
  , payMsgSource   = aMsgId
  , payStatus      = PaymentCreated
  , payUser        = user1
  , payType        = DebitFromUser
  , payMethod      = DwollaSettlement
  , payMethodId    = Nothing
  , payAmount      = Currency "USD" 10
  , payText        = "Tgthr Card"
  , paySubType     = NormalPayment
  , payACHInfo     = Nothing
  , payCreatedAt   = stringToTime "2019-11-01T20:15:32+00:00"
  , payFromJournal = Nothing
  , payToJournal   = Nothing
  }

newtype TestMonad a =
    TestMonad { unTestMonad :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      )

instance HasLedgerDB TestMonad where
  getUsersWithBalances _ = return [(user1, Currency "USD" 50)]
instance HasRandom TestMonad where
  aRandomUUID = return nil
instance HasPaymentsDB TestMonad where
  getPendingPaymentsOf _ theUser _ | theUser == user1 = return []
                                   | otherwise = error "What user is this?"
  savePayment _ _ = return ()
instance HasTime TestMonad where
  getCurrentTime = return $ stringToTime "2019-11-01T20:15:32+00:00"

spec :: Spec
spec = parallel $ do
  it "works on obvious credit case" $ do
    trace    <- midToTrace aMsgId
    payments <- unTestMonad $ schedulePaymentsFromLedger trace
    payments `shouldBe` [] -- No longer do we send a credit
