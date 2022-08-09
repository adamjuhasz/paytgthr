{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Redundant <&>" -}
{-# LANGUAGE RecordWildCards, OverloadedStrings, StrictData #-}

module LandingPage.Handlers.Transactions.Renderers.Transaction where

import           Control.Monad                  ( when )
import           Data.Functor                   ( (<&>) )
import           Data.List                      ( null
                                                , find
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Time.Format               ( formatTime
                                                , defaultTimeLocale
                                                )
import           Text.Blaze.Html5               ( (!)
                                                , toHtml
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Printf                    ( printf )
import           Shared.Models.Currency         ( getIsoCode
                                                , getMonetaryValue
                                                , Currency(..)
                                                )
import           Shared.Models.Transaction      ( cardMCCToA
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                , TransactionState(..)
                                                )
import           Shared.Models.User             ( UserID
                                                , UserModel(..)
                                                )
import           Shared.Transactions.Emoji      ( convertMCCToEmoji )

cents :: Currency -> Int
cents c =
  let x :: Double = fromRational $ getMonetaryValue c
      d           = dollars c
  in  floor ((x - fromIntegral d) * 100)

dollars :: Currency -> Int
dollars c = let x :: Double = fromRational $ getMonetaryValue c in floor x

usersRatio :: UserID -> Transaction -> Double
usersRatio uid Transaction {..} =
  let split = lookup uid trxSplitAmounts in maybe 100 fromRational split

partnersRatio :: UserID -> Transaction -> Double
partnersRatio uid Transaction {..} =
  case find ((/= uid) . fst) trxSplitAmounts of
    Nothing -> 100
    Just x  -> fromRational $ snd x

isPending :: Transaction -> Bool
isPending Transaction {..} =
  case (trxState, Data.List.null trxBillingAmounts) of
    (TrxCreated        , _    ) -> True
    (TrxAuthorized     , _    ) -> True
    (TrxPending        , _    ) -> True
    (TrxPendingReversal, _    ) -> True
    (TrxDeclined _     , _    ) -> False
    (TrxCompleted      , True ) -> True
    (TrxCompleted      , False) -> False
    (TrxDisputed _     , _    ) -> False

isDeclined :: Transaction -> Bool
isDeclined Transaction {..} = case trxState of
  TrxDeclined _ -> True
  _             -> False

transactionDetails :: UserID -> Transaction -> UserModel -> H.Html
transactionDetails thisUser trx@Transaction {..} UserModel {..} = do
  let trxMCC          = sum (trxMerchant <&> cmiMcc >>= cardMCCToA)
      purchasedAt     = formatTime defaultTimeLocale "%b %-e" trxPurchasedAt
      purchasedBy     = fromMaybe "Partner" usrFirstName
      desciption      = fromMaybe "Pending" trxDescription
      priceDollars    = dollars trxDisplayAmount
      priceCents      = "." <> printf "%02d" (cents trxDisplayAmount) :: String
      amountsCurrency = getIsoCode trxDisplayAmount
      userRatio       = usersRatio thisUser trx
      displayURatio   = printf "%2.0f" userRatio :: String
      partnerRatio    = partnersRatio thisUser trx
      displayPRatio   = printf "%2.0f" partnerRatio :: String
      currencyRatio   = Currency amountsCurrency (toRational userRatio / 100)
      userShare       = currencyRatio * trxDisplayAmount
      shareDollars    = dollars userShare
      shareCents      = "." <> printf "%02d" (cents userShare) :: String

  H.div ! A.class_ "category" $ H.preEscapedToMarkup $ convertMCCToEmoji trxMCC
  H.div ! A.class_ "category-details-spacer" $ ""
  H.div ! A.class_ "details" $ do
    H.div ! A.class_ "d-cell date" $ toHtml purchasedAt
    H.div ! A.class_ "details-spacer" $ "|"
    H.div ! A.class_ "d-cell cardholder" $ toHtml purchasedBy
  H.div ! A.class_ "details-merchant-spacer" $ ""
  H.div ! A.class_ "merchant" $ toHtml desciption
  when (isPending trx)
       (H.div ! A.class_ "split transaction-info pending" $ "Pending")
  when (isDeclined trx)
       (H.div ! A.class_ "split transaction-info declined" $ "Declined")
  H.div ! A.class_ "merchant-price-spacer" $ ""
  let priceClasses = case (isPending trx, isDeclined trx) of
        (True , _    ) -> "price pending"
        (_    , True ) -> "price declined"
        (False, False) -> "price"
  H.div ! A.class_ priceClasses ! A.id "price-group" $ do
    H.div ! A.class_ "currency" $ "$"
    H.div ! A.class_ "amount" $ toHtml priceDollars
    H.div ! A.class_ "amount-detail" $ toHtml priceCents
  H.div ! A.class_ "price-share-spacer" $ ""
  let shareClasses = case (isPending trx, isDeclined trx) of
        (True , _    ) -> "share pending"
        (_    , True ) -> "share declined"
        (False, False) -> "share"
  H.div ! A.class_ shareClasses ! A.id "share-group" $ do
    "Your Share: "
    H.span ! A.class_ "share-amount" $ "$" <> toHtml shareDollars
    H.span ! A.class_ "share-amount-detail" $ toHtml shareCents
  H.div ! A.class_ "share-split-spacer" $ ""
  H.div ! A.class_ "split" $ toHtml displayURatio <> ":" <> toHtml displayPRatio
