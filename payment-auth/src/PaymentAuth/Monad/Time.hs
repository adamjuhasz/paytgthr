module PaymentAuth.Monad.Time where

import           Data.Time.Clock                ( UTCTime )

class Monad m => HasTime m where
  getCurrentTime :: m UTCTime
