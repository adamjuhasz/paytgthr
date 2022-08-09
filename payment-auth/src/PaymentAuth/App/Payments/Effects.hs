module PaymentAuth.App.Payments.Effects where

import           Shared.Models.Ledger.Entry     ( LedgerEntry )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentStatus
                                                )
import           Shared.Models.RiskScore        ( RiskScore )

type PrevPaymentStatus = PaymentStatus

data PaymentEffects
 = RiskWasUpdated RiskScore
 | LedgerWasUpdated LedgerEntry
 | PaymentWasCreated Payment
 | PaymentWasUpdated PrevPaymentStatus Payment
 | PaymentWasCancelled Payment
 deriving (Eq, Show)
