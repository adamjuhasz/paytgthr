module PaymentAuth.Monad.Random where

import           Data.UUID                      ( UUID )

class Monad m => HasRandom m where
  aRandomUUID :: m UUID
