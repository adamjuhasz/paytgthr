{-# LANGUAGE RecordWildCards #-}

module LandingPage.Handlers.Transactions.API where

import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Handlers.Transactions.Renderers.Transaction
                                                ( isDeclined
                                                , isPending
                                                , partnersRatio
                                                , usersRatio
                                                )
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUserNotClosedHTTP
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv )
import           Shared.Models.Currency         ( Currency(..)
                                                , getIsoCode
                                                , getMonetaryValue
                                                , roundDownUSD
                                                )

import           Shared.Models.Transaction      ( MerchantInfo(cmiMcc)
                                                , Transaction(..)
                                                , cardMCCToA
                                                )
import           Shared.Models.User             ( UserModel(..) )
import           Shared.Transactions.Emoji      ( convertMCCToEmoji )
import           Shared.WebAPI.PaymentAuth.Client
                                                ( Routes(..)
                                                , paymentauthRoutes
                                                )
import           Text.Printf                    ( printf )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , json
                                                , liftAndCatchIO
                                                , request
                                                , setHeader
                                                )

type AccountsEnv = ClientEnv
type PayAuthEnv = ClientEnv

transactionAPIResponse
  :: V.Key SessionData -> AccountsEnv -> PayAuthEnv -> ActionM ()
transactionAPIResponse sKey accountsEnv payAuthEnv = do
  (uid, usermodel) <-
    request
    <&> vault
    <&> V.lookup sKey
    <&> expectSession
    >>= requireUserNotClosedHTTP accountsEnv

  trace <- createTrace

  let getLastTransactions =
        _GetPurchases (paymentauthRoutes payAuthEnv) trace uid 100
  trxs <- liftAndCatchIO getLastTransactions

  let currToDbl :: Currency -> Double
      currToDbl = fromRational . getMonetaryValue

  let dblToStr :: Double -> String
      dblToStr = printf "%.2f"
  let
    normalized = fmap
      (\trx@Transaction {..} ->
        let
          mcc             = sum (trxMerchant >>= cardMCCToA . cmiMcc)
          purchasedAt     = formatTime defaultTimeLocale "%b %-e" trxPurchasedAt
          purchasedById   = trxUserId
          desciption      = fromMaybe "Pending" trxDescription
          roundedAmount   = roundDownUSD trxDisplayAmount
          amounts         = dblToStr $ currToDbl roundedAmount
          amountsCurrency = getIsoCode trxDisplayAmount
          userRatio       = usersRatio (usrUserID usermodel) trx
          displayURatio   = printf "%2.0f" userRatio :: String
          partnerRatio    = partnersRatio (usrUserID usermodel) trx
          displayPRatio   = printf "%2.0f" partnerRatio :: String
          currencyRatio = Currency amountsCurrency (toRational userRatio / 100)
          userShare = dblToStr . currToDbl $ currencyRatio * trxDisplayAmount
        in
          object
            [ "mcc" .= mcc
            , "mccEmoji" .= convertMCCToEmoji mcc
            , "purchasedAt" .= purchasedAt
            , "purchasedById" .= purchasedById
            , "description" .= desciption
            , "isPending" .= isPending trx
            , "isDeclined" .= isDeclined trx
            , "amount" .= amounts
            , "currency" .= amountsCurrency
            , "userRatio" .= userRatio
            , "userRatio" .= displayURatio
            , "partnerRatio" .= displayPRatio
            , "userShare" .= userShare
            , "id" .= trxId
            ]
      )
      trxs

  now <- liftAndCatchIO getCurrentTime

  setHeader "cache-control" "private, max-age=1"
  Scotty.json $ object
    ["transactions" .= normalized, "date" .= now, "version" .= (1 :: Int)]
