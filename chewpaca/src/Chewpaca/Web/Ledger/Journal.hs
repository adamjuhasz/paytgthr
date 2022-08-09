{-# LANGUAGE RecordWildCards #-}
{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module Chewpaca.Web.Ledger.Journal where

import           Chewpaca.Tailwind.Classes      ( dataTable
                                                , indigoColorButton
                                                , mediumButton
                                                , tableCellClasses
                                                , tableFirstCellClasses
                                                , tableHeaderCellClasses
                                                , tableLinkClasses
                                                )
import           Chewpaca.Tailwind.Frame        ( showCurr
                                                , showDateTime
                                                )
import           Chewpaca.Web.Utils             ( uuidToStrStart )
import           Control.Monad                  ( forM_ )
import           Shared.Models.Ids              ( JournalId(JournalId)
                                                , LedgerEntryId(LedgerEntryId)
                                                , LedgerTrxId(LedgerTrxId)
                                                , UserID(UserID)
                                                )
import           Shared.Models.Ledger.Journal   ( JournalType(..)
                                                , LedgerJournal(..)
                                                )
import           Shared.Models.Payment          ( PaymentMethod
                                                  ( DwollaACH
                                                  , DwollaSettlement
                                                  )
                                                )
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                , toHtml
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderAJournalList :: [LedgerJournal] -> Html
renderAJournalList journals = do
  dataTable $ do
    H.table ! A.class_ "min-w-full divide-y divide-gray-200" $ do
      H.thead $ do
        H.tr $ do
          H.th ! A.class_ tableHeaderCellClasses $ "Id"
          H.th ! A.class_ tableHeaderCellClasses $ "Name"
          H.th ! A.class_ tableHeaderCellClasses $ "Rev"
          H.th ! A.class_ tableHeaderCellClasses $ "Last Entry"
          H.th ! A.class_ tableHeaderCellClasses $ "Balance"
          H.th ! A.class_ tableHeaderCellClasses $ "Pending Balance"
          H.th ! A.class_ tableHeaderCellClasses $ "Updated"
          H.th ! A.class_ tableHeaderCellClasses $ "LDG Trx"
          H.th ! A.class_ tableHeaderCellClasses $ "Type"
          H.th ! A.class_ tableHeaderCellClasses $ "User"
      H.tbody $ do
        forM_ journals $ \LedgerJournal {..} -> do
          H.tr ! A.class_ "bg-white" $ do
            H.td ! A.class_ tableFirstCellClasses $ do
              H.a ! A.class_ tableLinkClasses $ do
                toHtml $ uuidToStrStart journalId
            H.td ! A.class_ tableCellClasses $ do
              toHtml journalName
            H.td ! A.class_ tableCellClasses $ do
              toHtml journalRevision
            H.td ! A.class_ tableCellClasses $ do
              toHtml $ uuidToStrStart lastJournalEntry
            H.td ! A.class_ tableCellClasses $ do
              toHtml $ showCurr journalBalance
            H.td ! A.class_ tableCellClasses $ do
              toHtml $ showCurr journalPendingBalance
            H.td ! A.class_ tableCellClasses $ do
              toHtml $ showDateTime journalUpdated
            H.td ! A.class_ tableCellClasses $ do
              toHtml $ uuidToStrStart journalTransaction
            H.td ! A.class_ tableCellClasses $ do
              toHtml $ case journalType of
                PayTgthr        _                -> "PayTgthr"
                PayTgthrRewards _                -> "PayTgthrRewards"
                StashTgthr      _                -> "StashTgthr"
                SaveTgthr       _                -> "SaveTgthr"
                SecurtyDeposit  _                -> "SecurtyDeposit"
                FundingSource _ DwollaSettlement -> "FundingSource DwollaSettlement"
                FundingSource _ (DwollaACH pm)   -> "FundingSource " <> pm
                ExternalAccount DwollaSettlement -> "ExternalAccount DwollaSettlement"
                ExternalAccount (DwollaACH pm)   -> "ExternalAccount " <> pm
                VirtualAccount                   -> "VirtualAccount"
            H.td ! A.class_ tableCellClasses $ do
              toHtml $ case journalUser of
                Nothing          -> mempty
                Just (UserID uu) -> uuidToStrStart uu

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderJournalList :: [LedgerJournal] -> [LedgerJournal] -> Html
renderJournalList extJournals virtJournals = do

  H.div $ do
    H.h3 ! A.id "ledger" ! A.class_ "text-lg leading-6 font-medium text-gray-900" $ do
      "Find Journal"
    H.div $ do
      H.form ! A.method "post" ! A.action "/ledger/search" $ do
        H.select ! A.name "jType" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5" $ do
          H.option ! A.value "PayTgthr" $ "Pay Tgthr (need User)"
          H.option ! A.value "StashTgthr" $ "Stash Tgthr (need User)"
          H.option ! A.value "SaveTgthr" $ "Save Tgthr (need User)"
          H.option ! A.value "FundingSource" $ "Funding Source (need User)"
          H.option ! A.value "ExternalAccount" $ "External Account"
          H.option ! A.value "VirtualAccount" $ "Virtual Account"

        H.label ! A.for "uid" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
          "UserID (if needed)"
        H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
          H.input ! A.name "uid" ! A.id "uid" ! A.placeholder "" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"

        H.div ! A.class_ "mt-2" $ do
          H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
            H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
              "Find Journal"

  H.div $ do
    H.h3 ! A.id "ledger" ! A.class_ "text-lg leading-6 font-medium text-gray-900" $ do
      "External Account Journals"
    renderAJournalList extJournals

    H.div $ H.form ! A.method "post" ! A.action "/ledger/journals/external" $ do
      H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
        H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
          -- First line
          H.label ! A.for "name" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Account Name"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "name" ! A.id "name" ! A.placeholder "Settlement Account" ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
          H.label ! A.for "dwolla" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Dwolla ID"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "dwolla" ! A.id "dwolla" ! A.placeholder "https://..." ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"

          --Submit
          H.div ! A.class_ "mt-2" $ do
            H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                "New external account journal"

  H.div $ do
    H.h3 ! A.id "ledger" ! A.class_ "text-lg leading-6 font-medium text-gray-900" $ do
      "Virtual Journals"
    renderAJournalList virtJournals

    H.div $ H.form ! A.method "post" ! A.action "/ledger/journals/virtual" $ do
      H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
        H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
          -- First line
          H.label ! A.for "name" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Account Name"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "name" ! A.id "name" ! A.placeholder "Super Account" ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
            H.input ! A.name "dwolla" ! A.id "dwolla" ! A.value "" ! A.type_ "hidden"

          --Submit
          H.div ! A.class_ "mt-2" $ do
            H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                "New virtual journal"

