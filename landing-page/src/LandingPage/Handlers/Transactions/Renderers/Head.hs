{- HLINT ignore "Redundant do" -}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module LandingPage.Handlers.Transactions.Renderers.Head where

import           Control.Monad                  ( when )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import           LandingPage.Handlers.Transactions.Types
                                                ( TrxTemplate(..)
                                                , calculateNextLink
                                                , calculatePrevLink
                                                )
import           Shared.Models.Transaction      ( Transaction
                                                  ( trxDescription
                                                  , trxPurchasedAt
                                                  )
                                                )
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

basics :: H.Html
basics = do
  H.meta ! A.name "theme-color" ! A.content "#FFFFFF"
  H.link ! A.rel "stylesheet" ! A.href "/css/app.css"

transactionHead :: TrxTemplate -> H.Html
transactionHead EmptyTransaction{} = do
  H.title "Tgthr Card"
  basics

transactionHead tmpl@SafeSpend{} = do
  H.title "Tgthr Card"
  basics
  H.link ! A.rel "prefetch" ! A.href (calculateNextLink tmpl)

transactionHead tmpl@TransactionTemplate {..} = do
  let desc         = fromMaybe "" $ trxDescription currentTransaction
      purchaseTime = formatTime defaultTimeLocale "%b %-e"
        $ trxPurchasedAt currentTransaction

  H.title $ H.toHtml desc <> " - " <> H.toHtml purchaseTime
  basics
  when (isJust prevTransaction)
       (H.link ! A.rel "prefetch" ! A.href (calculatePrevLink tmpl))
  when (isJust nextTransaction)
       (H.link ! A.rel "prefetch" ! A.href (calculateNextLink tmpl))
