module PaymentAuth.Plaid.LinkTokens where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           PaymentAuth.Monad.Plaid        ( HasPlaidDB
                                                  ( getPlaidClientId
                                                  , getPlaidEnv
                                                  , getPlaidSecret
                                                  )
                                                )
import           PaymentAuth.Plaid.API          ( CreateLinkTokenBody(..)
                                                , CreateLinkTokenResponse(..)
                                                , PlaidRoutes(..)
                                                , plaidIOM
                                                )
import           Shared.Models.User             ( UserID )
import           Shared.WebAPI.PaymentAuth.API  ( GetPlaidLinkTokenResponse(..)
                                                , TraceContext
                                                )

getLinkToken
  :: (HasPlaidDB m, MonadIO m)
  => TraceContext
  -> UserID
  -> m GetPlaidLinkTokenResponse
getLinkToken _trace uid = do
  env      <- getPlaidEnv
  clientId <- getPlaidClientId
  secret   <- getPlaidSecret

  let createToken = _CreateLinkToken (plaidIOM env) clientId secret
        $ CreateLinkTokenBody uid

  CreateLinkTokenResponse token _ _ <- liftIO createToken

  return $ GetPlaidLinkTokenResponse token
