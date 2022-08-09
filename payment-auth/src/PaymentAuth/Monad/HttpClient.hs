module PaymentAuth.Monad.HttpClient where

import           PaymentAuth.Plaid              ( Requester )

class Monad m => HasHttpClient m where
  getHTTPClient :: m Requester
