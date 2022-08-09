{-# LANGUAGE NamedFieldPuns #-}

module LandingPage.Handlers.Application.Payments where

import           Data.Aeson                     ( Value(Null) )
import           Data.Functor                   ( (<&>) )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUserNotClosedHTTP
                                                )
import           Network.HTTP.Types             ( status500 )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.Payment
import           Shared.WebAPI.PaymentAuth.Client
                                                ( Routes(..)
                                                , payAuthClientM
                                                )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , json
                                                , liftAndCatchIO
                                                , request
                                                , status
                                                )

type AccountsEnv = ClientEnv
type PayAuthEnv = ClientEnv

redactPayment :: Payment -> Payment
redactPayment p = p { payMethodId = Just "<Redacted>" }

getRecentPayments
  :: V.Key SessionData -> AccountsEnv -> PayAuthEnv -> ActionM ()
getRecentPayments sKey accountsEnv payAuthEnv = do
  (uid, _) <-
    request
    <&> vault
    <&> V.lookup sKey
    <&> expectSession
    >>= requireUserNotClosedHTTP accountsEnv

  trace <- createTrace

  let fn = _GetPayments
        payAuthClientM
        trace
        uid
        [ PaymentCreated
        , PaymentPending
        , PaymentCompleted
        , PaymentFailed ACHR01
        , PaymentFailed ACHR02
        , PaymentFailed ACHR03
        , PaymentFailed ACHR04
        , PaymentFailed ACHR08
        , PaymentFailed ACHR09
        , PaymentFailed ACHR16
        , PaymentFailed ACHR20
        , PaymentFailed ACHR29
        ]
  res <- liftAndCatchIO $ runClientM fn payAuthEnv
  case res of
    Right payments ->
      Scotty.json
        . fmap redactPayment
        . take 20
        . filter
            (\Payment { paySubType } -> case paySubType of
              InitialVerification -> False
              RefundVerification  -> False
              NormalPayment       -> True
            )
        $ payments
    Left e -> do
      liftAndCatchIO $ putStr "Error: getPendingPayments _GetPayments " >> print
        (uid, e)
      status status500 >> Scotty.json Null
