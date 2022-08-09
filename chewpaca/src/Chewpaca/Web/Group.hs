{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# Language RecordWildCards #-}
{-# Language NamedFieldPuns #-}

module Chewpaca.Web.Group where

import           Chewpaca.Web.Groups            ( extGroupId
                                                , renderGroupStateBubble
                                                , renderMembers
                                                , renderSplits
                                                , renderStartDate
                                                , renderEndingDate
                                                )
import           Chewpaca.Tailwind.Classes      ( tableHeaderCellClasses
                                                , tableFirstCellClasses
                                                , tableCellClasses
                                                , dataTable
                                                , mediumButton
                                                , indigoColorButton
                                                , redColorButton
                                                )
import           Chewpaca.Tailwind.Frame        ( showDateTime )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( nil
                                                , toText
                                                )
import           Shared.Models.Group            ( GroupModel(..)
                                                , GroupId(..)
                                                , GroupMember(..)
                                                )
import           Shared.Models.User             ( UserID(..) )
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                , toHtml
                                                , toValue
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderGroup :: [(GroupModel, UTCTime)] -> Html
renderGroup []                           = "Error: no groups in list"
renderGroup groups@(mostRecentGroup : _) = do
  H.div ! A.id "top" $ mempty

  renderGroupCard mostRecentGroup

  H.div $ do
    H.h3 ! A.id "revisions" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Revisions"
    renderRevisions groups

  H.div $ do
    H.h3 ! A.id "actions" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "User Actions"
    renderGroupActions (fst mostRecentGroup)

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderGroupCard :: (GroupModel, UTCTime) -> Html
renderGroupCard (GroupModel {..}, lastUpdate) = do
  let cardLeftText  = "text-sm leading-5 font-medium text-gray-500"
      cardRightText = "mt-1 text-sm leading-5 text-gray-900 sm:mt-0 sm:col-span-2"
      cardDataRow   = "mt-8 sm:mt-0 sm:grid sm:grid-cols-3 sm:gap-4 sm:border-t sm:border-gray-200 sm:px-6 sm:py-5"
  H.div ! A.class_ "bg-white shadow overflow-hidden sm:rounded-lg" $ do
    H.div ! A.class_ "px-4 py-5 sm:p-0" $ do
      H.dl $ do
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Id"
          H.dd ! A.class_ cardRightText $ do
            toHtml $ extGroupId grpId
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Last updated at"
          H.dd ! A.class_ cardRightText $ do
            showDateTime lastUpdate
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "State"
          H.dd ! A.class_ cardRightText $ do
            renderGroupStateBubble grpStatus
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Revision"
          H.dd ! A.class_ cardRightText $ do
            toHtml grpRevision
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Split"
          H.dd ! A.class_ cardRightText $ do
            mapM_ (renderSplits (UserID nil)) grpSplit
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Members"
          H.dd ! A.class_ cardRightText $ do
            mapM_ (renderMembers (UserID nil)) grpMembers

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderRevisions :: [(GroupModel, UTCTime)] -> Html
renderRevisions groups = do
  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "Last update"
        H.th ! A.class_ tableHeaderCellClasses $ "State"
        H.th ! A.class_ tableHeaderCellClasses $ "Rev"
        H.th ! A.class_ tableHeaderCellClasses $ "Start"
        H.th ! A.class_ tableHeaderCellClasses $ "End"
        H.th ! A.class_ tableHeaderCellClasses $ "Members"
        H.th ! A.class_ tableHeaderCellClasses $ "Split"
    H.tbody $ do
      let createRow (GroupModel {..}, time) = do
            H.tr ! A.id (toValue $ "group-" <> extGroupId grpId) ! A.class_ "bg-white" $ do
              H.td ! A.class_ tableFirstCellClasses $ do
                showDateTime time
              H.td ! A.class_ tableCellClasses $ do
                renderGroupStateBubble grpStatus
              H.td ! A.class_ tableCellClasses $ do
                toHtml grpRevision
              H.td ! A.class_ tableCellClasses $ do
                renderStartDate grpStart
              H.td ! A.class_ tableCellClasses $ do
                renderEndingDate grpEnd
              H.td ! A.class_ tableCellClasses $ do
                mapM_ (renderMembers (UserID nil)) grpMembers
              H.td ! A.class_ tableCellClasses $ do
                mapM_ (renderSplits (UserID nil)) grpSplit

      mapM_ createRow groups

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderGroupActions :: GroupModel -> Html
renderGroupActions GroupModel { grpId = GroupId g, grpMembers } = do
  let gid = toValue $ toText g
  H.div ! A.class_ "bg-white shadow overflow-hidden sm:rounded-md" $ do
    H.ul $ do
      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "Close thre group, make sure users have a backup group"
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/group/" <> gid <> "/close"
              H.form ! A.action url ! A.method "post" $ do
                H.input ! A.type_ "hidden" ! A.name "groupid" ! A.value gid
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> redColorButton) $ do
                  "Close group"

      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "Activate group wth 50/50 split"
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/group/" <> gid <> "/resetsplit"
              H.form ! A.action url ! A.method "post" $ do
                H.input ! A.type_ "hidden" ! A.name "groupid" ! A.value gid
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                  "Activate group"

      mapM_
        (\GroupMember { mbrUser = UserID u } -> do
          let uid = toValue $ toText u
          H.li $ do
            H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
              H.div ! A.class_ "flex items-center justify-between" $ do
                H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
                  H.span "User "
                  H.span . toHtml $ toText u
                H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
                  H.div $ do
                    let url = "/group/" <> gid <> "/acceptmember/" <> uid
                    H.form ! A.action url ! A.method "post" $ do
                      H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                        "Force group invite accept"
                  H.br
                  H.div $ do
                    let url = "/group/" <> gid <> "/approvesplit/" <> uid
                    H.form ! A.action url ! A.method "post" $ do
                      H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                        "Force split aproval"
        )
        grpMembers

