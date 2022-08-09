{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# Language RecordWildCards #-}

module Chewpaca.Web.Groups where

import           Chewpaca.Web.Users             ( generateLinkToUserPage
                                                , renderUserID
                                                )
import           Chewpaca.Tailwind.Classes      ( badgeClasses
                                                , dataTable
                                                , tableHeaderCellClasses
                                                , tableFirstCellClasses
                                                , tableCellClasses
                                                , tableLinkClasses
                                                )
import           Chewpaca.Tailwind.Frame        ( showDateTime )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( toText
                                                , nil
                                                )
import           Shared.Models.Group            ( GroupId(..)
                                                , GroupStatus(..)
                                                , GroupSplit(..)
                                                , GroupMember(..)
                                                , GroupModel(..)
                                                )
import           Shared.Models.User             ( UserID(..) )
import           Text.Blaze.Html5               ( (!)
                                                , AttributeValue
                                                , Html
                                                , toHtml
                                                , toValue
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

renderGroupId :: GroupId -> Html
renderGroupId (GroupId g) = case T.splitOn "-" $ toText g of
  []             -> "Error: can't split groupID"
  (firstSec : _) -> toHtml (firstSec <> "...")

generateLinkToGroup :: GroupId -> AttributeValue
generateLinkToGroup (GroupId g) = toValue $ "/group/" <> toText g

extGroupId :: GroupId -> Text
extGroupId (GroupId g) = toText g

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderGroupStateBubble :: GroupStatus -> Html
renderGroupStateBubble GroupCreated = H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Group Created"
renderGroupStateBubble GroupPending = H.span ! A.class_ (badgeClasses <> " bg-blue-100 text-blue-800") $ "Group Pending"
renderGroupStateBubble GroupActive  = H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800") $ "Group Active"
renderGroupStateBubble GroupPaused  = H.span ! A.class_ (badgeClasses <> " bg-yellow-100 text-yellow-800") $ "Group Paused"
renderGroupStateBubble GroupClosed  = H.span ! A.class_ (badgeClasses <> " bg-yellow-100 text-yellow-800") $ "Group Closed"
renderGroupStateBubble GroupExpired = H.span ! A.class_ (badgeClasses <> " bg-yellow-100 text-yellow-800") $ "Group Expired"
renderGroupStateBubble GroupDenied  = H.span ! A.class_ (badgeClasses <> " bg-yellow-100 text-yellow-800") $ "Group Denied"

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderSplits :: UserID -> GroupSplit -> Html
renderSplits self GroupSplit {..} = do
  H.div ! A.class_ "my-1" $ do
    let color = if splApproved then "bg-green-100 text-green-800" else "bg-yellow-100 text-yellow-800"
    H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ do
      toHtml (ceiling (fromRational splRatio :: Double) :: Int)

    H.span " "

    H.span ! A.class_ (badgeClasses <> color) $ do
      if splApproved then "Approved" else "Not approved"

    H.span " "

    if splUser == self
      then "self"
      else do
        H.a ! A.href (generateLinkToUserPage splUser) ! A.class_ tableLinkClasses $ do
          renderUserID splUser

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderMembers :: UserID -> GroupMember -> Html
renderMembers self GroupMember {..} = do
  H.div ! A.class_ "my-1" $ do
    let color = if mbrAccepted then "bg-green-100 text-green-800" else "bg-yellow-100 text-yellow-800"

    H.span ! A.class_ (badgeClasses <> color) $ do
      if mbrAccepted then "Accepted" else "Waiting on"

    H.span " "

    if mbrUser == self
      then "self"
      else do
        H.a ! A.href (generateLinkToUserPage mbrUser) ! A.class_ tableLinkClasses $ do
          renderUserID mbrUser

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderStartDate :: Maybe UTCTime -> Html
renderStartDate time = case time of
  Nothing -> H.span ! A.class_ (badgeClasses <> "bg-gray-100 text-gray-800") $ "Permanent"
  Just t  -> showDateTime t

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderEndingDate :: Maybe UTCTime -> Html
renderEndingDate time = case time of
  Nothing -> mempty
  Just t  -> showDateTime t

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderGroups :: [(GroupModel, UTCTime)] -> Html
renderGroups groups = do
  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "Last update"
        H.th ! A.class_ tableHeaderCellClasses $ "Id"
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
                H.a ! A.href (generateLinkToGroup grpId) ! A.class_ tableLinkClasses $ do
                  renderGroupId grpId
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

