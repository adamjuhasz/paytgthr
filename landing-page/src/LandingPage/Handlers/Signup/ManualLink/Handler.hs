{-|

Manual ACH Link handler
 -}

{- HLINT ignore "Redundant <&>" -}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module LandingPage.Handlers.Signup.ManualLink.Handler where

import           Control.Monad                  ( unless )
import           Data.Aeson                     ( FromJSON
                                                , KeyValue((.=))
                                                , ToJSON
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Vault.Lazy               as V
import           GHC.Generics                   ( Generic )
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( AcceptType(..)
                                                , createTrace
                                                , expectSession
                                                , getAcceptType
                                                , requireUserNotClosedHTTP
                                                )
import           LandingPage.Validators         ( isFixedLengthNumber
                                                , isNotEmpty
                                                )
import           Network.HTTP.Types             ( status400 )
import           Network.HTTP.Types.Status      ( Status(..)
                                                , status403
                                                , status500
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , ClientError(..)
                                                , ResponseF(..)
                                                , runClientM
                                                )
import           Shared.Models.User             ( RedactedText(..) )
import           Shared.WebAPI.PaymentAuth.Client
                                                ( Routes(..)
                                                , SetManualFSBody(..)
                                                , VerifyFSBankBody(..)
                                                , VerifyFSBankResponse(..)
                                                , payAuthClientM
                                                , paymentauthRoutes
                                                )
import           Text.Read                      ( readMaybe )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , finish
                                                , json
                                                , liftAndCatchIO
                                                , next
                                                , param
                                                , request
                                                , rescue
                                                , status
                                                )

type AccountsClient = ClientEnv
type PayAuthClient = ClientEnv

data EntryError
  = BadRouting
  | BadAccountNumber
  | BadBankName
  | BadAccountName
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data VerifyError
  = NoVerifyError
  | BadAmount
  | BadFormat
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

handleEntry
  :: V.Key SessionData -> AccountsClient -> PayAuthClient -> ActionM ()
handleEntry sKey accountsEnv payAuthEnv = do
  (user, _) <-
    request
    <&> vault
    <&> V.lookup sKey
    <&> expectSession
    >>= requireUserNotClosedHTTP accountsEnv

  bank :: Text    <- param "bank"
  name :: Text    <- param "name"
  routing :: Text <- param "routing"
  acctNum :: Text <- param "accountNum"

  let routingError = isNothing $ isFixedLengthNumber 9 routing
  let bankError    = isNothing $ isNotEmpty bank
  let nameError    = isNothing $ isNotEmpty name
  let acctError    = T.length acctNum < 4 || T.length acctNum > 17
  let inputErrors =
        [ BadRouting | routingError ]
          <> [ BadBankName | bankError ]
          <> [ BadAccountName | nameError ]
          <> [ BadAccountNumber | acctError ]

  let badInputReponse = do
        status status400
        Scotty.json (object ["errors" .= inputErrors])
        finish
  unless (null inputErrors) badInputReponse

  let body = SetManualFSBody { unverifiedABARouting  = RedactedText routing
                             , unverifiedDDAAccount  = RedactedText acctNum
                             , unverifiedAccountName = name
                             , unverifiedBankName    = bank
                             }

  trace <- createTrace
  let addManualFS = _SetFSBankManual payAuthClientM trace user body

  res <- liftAndCatchIO $ runClientM addManualFS payAuthEnv
  case res of
    Left (FailureResponse _ Response { responseStatusCode = Status { statusCode = 403 }, responseBody = resBody })
      -> do
        liftAndCatchIO $ putStr "Info: _SetFSBankManual 403 " >> print
          (user, resBody, body)
        status status403 >> Scotty.json (object ["success" .= False]) >> finish
    Left e -> do
      liftAndCatchIO $ putStr "Error: _SetFSBankManual " >> print
        (user, e, body)
      status status500 >> finish
    Right _ -> return ()

  accepts <- getAcceptType
  case accepts of
    ApplicationJSON -> Scotty.json $ object ["success" .= True]
    TextHTML        -> Scotty.next

handleVerification
  :: V.Key SessionData -> AccountsClient -> PayAuthClient -> ActionM ()
handleVerification sKey acountsEnv payAuthEnv = do
  let rescueMaybe :: a -> ActionM (Maybe b)
      rescueMaybe _ = return Nothing

  (user, _) <-
    request
    <&> vault
    <&> V.lookup sKey
    <&> expectSession
    >>= requireUserNotClosedHTTP acountsEnv

  inputM :: Maybe Text <- (Just <$> param "amount1") `rescue` rescueMaybe
  let amountM = case inputM of
        Nothing -> Nothing
        Just t  -> readMaybe $ T.unpack ("0" <> t) -- ".17" fails otherwise

  let badInputReponse = do
        status status400
        Scotty.json $ object ["errors" .= [BadFormat]]
        finish
  amount <- maybe badInputReponse return amountM
  let normalizedAmount = if amount > 100 then amount / 100 else amount

  trace <- createTrace
  let verifyDepsoit =
        _VerifyBankAccount (paymentauthRoutes payAuthEnv) trace user
          $ VerifyFSBankBody [normalizedAmount]
  VerifyFSBankResponse {..} <- liftAndCatchIO verifyDepsoit

  liftAndCatchIO $ putStr "handleVerification: " >> print
    (user, amount, normalizedAmount, verificationSuccess)

  if verificationSuccess
    then do
      accepts <- getAcceptType
      case accepts of
        ApplicationJSON -> Scotty.json $ object ["success" .= True]
        TextHTML        -> Scotty.next
    else do
      status status400
      Scotty.json $ object ["errors" .= [BadAmount]]
      finish
