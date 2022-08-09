{-# LANGUAGE RecordWildCards #-}

{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module Chewpaca.Web.Rewards.Rewards where

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
import           Data.Aeson                     ( encode )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text.Encoding            as TE
import           Data.UUID                      ( toText )
import           Shared.Models.Ids
import           Shared.Models.Rewards.Boost
import           Shared.Models.Rewards.Categorizer
import           Shared.Models.Transaction
import           Text.Blaze.Html5               ( (!)
                                                , (!?)
                                                , Html
                                                , toHtml
                                                , toValue
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderABoost :: RewardBoost -> Html
renderABoost RewardBoost { boostId = RewardId i, ..} = do
  H.tr ! A.class_ "bg-white" $ do
    H.td ! A.class_ tableFirstCellClasses $ do
      H.a ! A.href ("/rewards/reward/" <> toValue (toText i)) ! A.class_ tableLinkClasses $ do
        toHtml $ uuidToStrStart i
    H.td ! A.class_ tableCellClasses $ do
      toHtml boostName
    H.td ! A.class_ tableCellClasses $ do
      toHtml boostRewardInBips
    H.td ! A.class_ tableCellClasses $ do
      toHtml boostExpiresInHr
    H.td ! A.class_ tableCellClasses $ do
      toHtml boostActive
    H.td ! A.class_ tableCellClasses $ do
      showDateTime boostUpdated

-- inline brittany config for width
-- brittany-next-binding --columns 1000
listRewards :: [RewardBoost] -> Html
listRewards boosts = do
  H.div $ do
    H.h3 ! A.id "ledger" ! A.class_ "text-lg leading-6 font-medium text-gray-900" $ do
      "Current rewards"
    dataTable $ do
      H.table ! A.class_ "min-w-full divide-y divide-gray-200" $ do
        H.thead $ do
          H.tr $ do
            H.th ! A.class_ tableHeaderCellClasses $ "Id"
            H.th ! A.class_ tableHeaderCellClasses $ "Name"
            H.th ! A.class_ tableHeaderCellClasses $ "Reward"
            H.th ! A.class_ tableHeaderCellClasses $ "Lifetime in Hr"
            H.th ! A.class_ tableHeaderCellClasses $ "Active"
            H.th ! A.class_ tableHeaderCellClasses $ "Updated"
        H.tbody $ do
          mapM_ renderABoost boosts

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderRewardMatch :: RewardBoost -> [Transaction] -> Html
renderRewardMatch boost@RewardBoost { boostId = RewardId i, ..} trxs = do
  let cardLeftText  = "text-sm leading-5 font-medium text-gray-500"
  let cardRightText = "mt-1 text-sm leading-5 text-gray-900 sm:mt-0 sm:col-span-2"
  let cardDataRow = "mt-8 sm:mt-0 sm:grid sm:grid-cols-3 sm:gap-4 sm:border-t sm:border-gray-200 sm:px-6 sm:py-5"

  H.div $ do
    H.h3 ! A.id "ledger" ! A.class_ "text-lg leading-6 font-medium text-gray-900" $ do
      "Details"
    H.form ! A.action ("/rewards/edit/" <> toValue (toText i)) ! A.method "post" $ do
      H.div ! A.class_ "bg-white shadow overflow-hidden sm:rounded-lg" $ do
        H.div ! A.class_ "px-4 py-5 sm:p-0" $ do
          H.dl $ do
            H.div ! A.class_ cardDataRow $ do
              H.dt ! A.class_ cardLeftText $ do
                "Id"
              H.dd ! A.class_ cardRightText $ do
                toHtml $ toText i

            H.div ! A.class_ cardDataRow $ do
              H.dt ! A.class_ cardLeftText $ do
                "Name"
              H.dd ! A.class_ cardRightText $ do
                H.input ! A.name "name" ! A.type_ "text" ! A.value (toValue boostName) ! A.class_ "form-input block sm:text-sm sm:leading-5"

            H.div ! A.class_ cardDataRow $ do
              H.dt ! A.class_ cardLeftText $ do
                "Active"
              H.dd ! A.class_ cardRightText $ do
                H.select ! A.name "active" ! A.class_ "block text-base sm:text-sm rounded-md" $ do
                  H.option !? (boostActive, A.selected "") $ "True"
                  H.option !? (not boostActive, A.selected "") $ "False"

            H.div ! A.class_ cardDataRow $ do
              H.dt ! A.class_ cardLeftText $ do
                "Reward (Bips)"
              H.dd ! A.class_ cardRightText $ do
                H.input ! A.name "reward" ! A.type_ "text" ! A.value (toValue boostRewardInBips) ! A.class_ "form-input block sm:text-sm sm:leading-5"

            H.div ! A.class_ cardDataRow $ do
              H.dt ! A.class_ cardLeftText $ do
                "Lifetime (Hours)"
              H.dd ! A.class_ cardRightText $ do
                H.input ! A.name "lifetime" ! A.type_ "text" ! A.value (toValue boostExpiresInHr) ! A.class_ "form-input block sm:text-sm sm:leading-5"

            H.div ! A.class_ cardDataRow $ do
              H.dt ! A.class_ cardLeftText $ do
                "Max payout per purchase"
              H.dd ! A.class_ cardRightText $ do
                toHtml $ showCurr boostMaximumPayout

            H.div ! A.class_ cardDataRow $ do
              H.dt ! A.class_ cardLeftText $ do
                "Matcher"
              H.dd ! A.class_ cardRightText $ do
                toHtml $ show boostMatch

            H.div ! A.class_ cardDataRow $ do
              H.dt ! A.class_ cardLeftText $ do
                "Matcher JSON"
              H.dd ! A.class_ cardRightText $ do
                H.textarea ! A.name "matcher" ! A.class_ "resize-y w-full form-textarea" ! A.rows "5" $ do
                  toHtml $ TE.decodeUtf8 $ BL.toStrict $ encode boostMatch

            H.div ! A.class_ cardDataRow $ do
              H.dt ! A.class_ cardLeftText $ do
                "Update with new settings"
              H.dd ! A.class_ cardRightText $ do
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                  "Update Reward"

  H.div $ do
    H.h3 ! A.id "ledger" ! A.class_ "text-lg leading-6 font-medium text-gray-900" $ do
      "Transaction matches"
    dataTable $ do
      H.table ! A.class_ "min-w-full divide-y divide-gray-200" $ do
        H.thead $ do
          H.tr $ do
            H.th ! A.class_ tableHeaderCellClasses $ "MCC"
            H.th ! A.class_ tableHeaderCellClasses $ "Merchant"
            H.th ! A.class_ tableHeaderCellClasses $ "Description"
        H.tbody $ do
          mapM_ (renderTranasaction boost) trxs

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderTranasaction :: RewardBoost -> Transaction -> Html
renderTranasaction RewardBoost{} Transaction { trxMerchant = Nothing } = do
  H.tr ! A.class_ "bg-white" $ do
    H.td ! A.class_ tableFirstCellClasses $ do
      "Bad Merchant"
renderTranasaction RewardBoost{} Transaction { trxDescription = Nothing } = do
  H.tr ! A.class_ "bg-white" $ do
    H.td ! A.class_ tableFirstCellClasses $ do
      "Bad Descriotion"
renderTranasaction boost trx@Transaction { trxMerchant = Just CardMerchant { cmiMcc = MastercardMCC mcc, ..}, trxDescription = Just descr } = do
  let background = case categorize trx boost of
        Nothing -> "bg-pink-300"
        Just _  -> "bg-green-300"
  H.tr ! A.class_ background $ do
    H.td ! A.class_ tableFirstCellClasses $ do
      toHtml mcc
    H.td ! A.class_ tableCellClasses $ do
      toHtml cmiName
    H.td ! A.class_ tableCellClasses $ do
      toHtml descr
