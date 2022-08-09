module PaymentAuth.Monad.EventTracking
  ( module PaymentAuth.Monad.EventTracking
  , object
  , (.=)
  ) where

import           Data.Aeson                     ( (.=)
                                                , Value
                                                , object
                                                )
import           Data.Text                      ( Text )
import           Shared.Models.Transaction      ( Transaction
                                                , TransactionState
                                                )
import           Shared.Models.User             ( UserID )
import           Shared.WebAPI.PaymentAuth.API  ( TraceContext )

type PreviousTransactionState = TransactionState

class Monad m => HasEventTracking m where
  trackEventWithProps     :: TraceContext -> UserID -> Text -> Value -> m ()
  trackEvent              :: TraceContext -> UserID -> Text -> m ()
  trackOneOffTrait        :: UserID -> [(Text, Value)] -> m ()
  trackTransactionChanged :: TraceContext -> PreviousTransactionState -> Transaction -> m ()
