
{-# LANGUAGE RecordWildCards #-}

module AFSM.DB.GroupSave
  ( saveGroup
  , saveACategorySplit
  ) where

import           AFSM.DB.GroupGet               ( getGroupDefaultSplit )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Time.Clock                ( getCurrentTime )
import           Database.PostgreSQL.Simple     ( Connection
                                                , execute
                                                , withTransaction
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( Action(EscapeIdentifier)
                                                , ToField(..)
                                                )

import           Shared.Models.CategorySplit    ( CategoryCode(Category000)
                                                , CategorySplit(..)
                                                , CategoryState(CategoryActive)
                                                , categorySplitFields
                                                )
import           Shared.Models.Group            ( GroupModel(..)
                                                , groupModelFields
                                                )

saveGroup :: GroupModel -> Connection -> IO ()
saveGroup group@GroupModel {..} conn = do
  withTransaction conn $ do
    _ <- execute conn insert group
    _ <- execute conn update group
    return ()

  currentSplit <- getGroupDefaultSplit grpId conn
  now          <- getCurrentTime
  let newCatSplit = case currentSplit of
        Nothing -> CategorySplit { groupId     = grpId
                                 , categoryId  = Category000
                                 , catRevision = 1
                                 , createdAt   = now
                                 , updatedAt   = now
                                 , splits      = grpSplit
                                 , state       = CategoryActive
                                 }
        Just c -> c { catRevision = catRevision c + 1
                    , splits      = grpSplit
                    , updatedAt   = now
                    }
  saveACategorySplit newCatSplit conn

  return ()
 where
  insert =
    "INSERT INTO tgthr.groups ( "
      <> fst groupModelFields
      <> ") VALUES ( "
      <> snd groupModelFields
      <> ")"
  update =
    "UPSERT INTO tgthr.groups_current_rev ( "
      <> fst groupModelFields
      <> ") VALUES ( "
      <> snd groupModelFields
      <> ")"

newtype JSONText = JSONText Text deriving (Eq, Show)
instance ToField JSONText where
  toField (JSONText t) = EscapeIdentifier . encodeUtf8 $ t

saveACategorySplit :: CategorySplit -> Connection -> IO ()
saveACategorySplit cat conn = withTransaction conn $ do
  _ <- execute conn insert cat
  _ <- execute conn update cat
  return ()
 where
  insert =
    "INSERT INTO tgthr.group_category_splits ( "
      <> fst categorySplitFields
      <> ") VALUES ( "
      <> snd categorySplitFields
      <> ")"
  update =
    "UPSERT INTO tgthr.group_category_splits_current_rev ( "
      <> fst categorySplitFields
      <> ") VALUES ( "
      <> snd categorySplitFields
      <> ")"

