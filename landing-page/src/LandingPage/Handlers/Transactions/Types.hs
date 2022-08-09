{-# LANGUAGE StrictData, RecordWildCards #-}

module LandingPage.Handlers.Transactions.Types where

import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import           Data.UUID                      ( nil
                                                , toText
                                                )
import           LandingPage.Types              ( UserSignupStep(Complete)
                                                , stepToPath
                                                )
import           Shared.Models.Transaction      ( Transaction
                                                , TransactionId(..)
                                                )
import           Shared.Models.User             ( UserID
                                                , UserModel
                                                )
import           Text.Blaze.Html5               ( toValue )
import qualified Text.Blaze.Html5              as H

data TrxTemplate
  = EmptyTransaction
    { recomendation :: H.Html
    }
  | TransactionTemplate
    { prevTransaction :: Maybe TransactionId
    , nextTransaction :: Maybe TransactionId
    , color :: Text
    , currentTransaction :: Transaction
    , purchaser :: UserModel
    , user :: UserID
    }
  | SafeSpend
    { accountName :: Text
    , spendable :: Double
    , hasTransactions :: Maybe TransactionId
    }

trxColors :: [Text]
trxColors = ["pink", "yellow", "green"]

nextColor :: Text -> Text
nextColor t = (cycle trxColors !!) . (+ 1) . fromJust . elemIndex t $ trxColors

prevColor :: Text -> Text
prevColor t =
  (cycle trxColors !!)
    . (+ (length trxColors - 1))
    . fromJust
    . elemIndex t
    $ trxColors

hrefBase :: Text
hrefBase = stepToPath Complete

calculatePrevLink :: TrxTemplate -> H.AttributeValue
calculatePrevLink template = case template of
  EmptyTransaction{} -> toValue hrefBase
  TransactionTemplate { prevTransaction = Just (TransactionId prevId), ..} ->
    toValue $ hrefBase <> "/" <> toText prevId <> "?color=" <> prevColor color
  TransactionTemplate{} -> toValue hrefBase
  SafeSpend{}           -> ""

calculateNextLink :: TrxTemplate -> H.AttributeValue
calculateNextLink template = case template of
  EmptyTransaction{} -> ""
  TransactionTemplate { nextTransaction = Just (TransactionId nextId), ..} ->
    toValue $ hrefBase <> "/" <> toText nextId <> "?color=" <> nextColor color
  TransactionTemplate{} -> ""
  SafeSpend { hasTransactions = Just (TransactionId nextId) } ->
    toValue $ hrefBase <> "/" <> toText nextId <> "?color=" <> "yellow"
  SafeSpend{} -> toValue $ hrefBase <> "/" <> toText nil
