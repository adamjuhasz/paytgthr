{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{- HLINT ignore "Reduce duplication" -}

{-|
Module      : Group List handler
Description : Get a list of all groups for a user
Maintainer  : adam@example.com
Stability   : experimental
-}

module LandingPage.Handlers.Application.GroupSplit where

import           Control.Monad                  ( unless
                                                , when
                                                )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromJust )
import           Data.UUID                      ( fromText
                                                , nil
                                                )
import qualified Data.Vault.Lazy               as V
import           GHC.Generics                   ( Generic )
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUser
                                                )
import           Network.HTTP.Types             ( status400
                                                , status403
                                                , status404
                                                , status500
                                                )
import           Network.Wai                    ( Request(vault) )
import           Servant                        ( NoContent(NoContent) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.CategorySplit    ( CategoryCode
                                                , CategorySplit(..)
                                                , CategoryState
                                                  ( CategoryActive
                                                  , CategoryDisabled
                                                  )
                                                )
import           Shared.Models.Group            ( GroupId(..)
                                                , GroupMember(mbrUser)
                                                , GroupModel
                                                  ( grpMembers
                                                  , grpSplit
                                                  )
                                                , GroupSplit
                                                  ( GroupSplit
                                                  , splApproved
                                                  , splRatio
                                                  , splUser
                                                  )
                                                )
import           Shared.Models.User             ( UserID )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( ChangeSplitBody(..)
                                                , Routes(..)
                                                , SetCatSplit(..)
                                                , asClientM
                                                , incrementTrace
                                                )
import           Text.Read                      ( readMaybe )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , finish
                                                , json
                                                , jsonData
                                                , liftAndCatchIO
                                                , param
                                                , request
                                                , status
                                                )

setSplit :: V.Key SessionData -> ClientEnv -> ActionM ()
setSplit sKey env = do
  uid <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  gid :: GroupId           <- GroupId . fromJust . fromText <$> param "id"
  splitChoiceStr :: String <- param "split"

  splitChoice :: Int       <- case readMaybe splitChoiceStr of
    Just (s :: Double) -> return (floor s :: Int)
    Nothing ->
      status status400
        >> Scotty.json (object [("error", "split not Int")])
        >> finish

  trace      <- createTrace
  groupE     <- liftAndCatchIO $ runClientM (_GroupGet asClientM trace gid) env
  groupModel <- case groupE of
    Left e ->
      status status404
        >> Scotty.json (object [("error", "group not found"), "msg" .= show e])
        >> finish
    Right g -> return g

  let members  = mbrUser <$> grpMembers groupModel
  let isMember = uid `elem` members

  unless
    isMember
    (status status403 >> Scotty.json (object [("error", "NotMember")]) >> finish
    )

  when
    (splitChoice < 10 || splitChoice > 90)
    (  status status400
    >> Scotty.json (object [("error", "IllegalValue")])
    >> finish
    )

  let otherUser       = head $ filter (/= uid) members
  let partnerApproved = extractApproval groupModel uid (/=)
  let thisSplit       = GroupSplit uid (toRational splitChoice) True
  let otherSplit =
        GroupSplit otherUser (toRational (100 - splitChoice)) partnerApproved

  trace' <- incrementTrace trace
  let fn = _GroupChangeSplit asClientM trace' gid
        $ ChangeSplitBody [thisSplit, otherSplit] uid
  res <- liftAndCatchIO $ runClientM fn env
  case res of
    Left e -> do
      liftAndCatchIO $ putStr "Error: setSplit _GroupChangeSplit " >> print
        (uid, gid, e)
      status status500 >> Scotty.json (object ["success" .= False])
    Right _ -> Scotty.json $ object ["success" .= True]

 where
  extractApproval :: GroupModel -> t -> (t -> UserID -> Bool) -> Bool
  extractApproval group user f =
    splApproved . head $ filter (f user . splUser) $ grpSplit group

data NewCategorySplits = NewCategorySplits
  { catcode       :: CategoryCode
  , catEnabled    :: Bool
  , thisUserSplit :: Int
  }
  deriving (Eq, Show, Generic)
instance FromJSON NewCategorySplits where
  parseJSON = withObject "NewCategorySplits" $ \o -> do
    catcode       <- o .: "id"
    catEnabled    <- o .: "enabled"
    thisUserSplit <- o .: "userSplit"
    return NewCategorySplits { .. }
instance ToJSON NewCategorySplits where
  toJSON NewCategorySplits {..} = object
    ["id" .= catcode, "enabled" .= catEnabled, "userSplit" .= thisUserSplit]

setCategorySplits :: V.Key SessionData -> ClientEnv -> ActionM ()
setCategorySplits sKey accountsEnv = do
  uid <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  gid :: GroupId <- GroupId . fromJust . fromText <$> param "id"
  newSplits :: [NewCategorySplits] <- Scotty.jsonData

  trace <- createTrace
  groupE <- liftAndCatchIO
    $ runClientM (_GroupGet asClientM trace gid) accountsEnv
  groupModel <- case groupE of
    Left e ->
      status status404
        >> Scotty.json (object [("error", "group not found"), "msg" .= show e])
        >> finish
    Right g -> return g

  let members   = mbrUser <$> grpMembers groupModel
  let isMember  = uid `elem` members
  let otherUser = head $ filter (/= uid) members

  unless
    isMember
    (status status403 >> Scotty.json (object [("error", "NotMember")]) >> finish
    )

  let setSplitObjs = fmap
        (\NewCategorySplits {..} -> SetCatSplit
          catcode
          [ GroupSplit uid       (toRational thisUserSplit)         True
          , GroupSplit otherUser (toRational $ 100 - thisUserSplit) True
          ]
          (if catEnabled then CategoryActive else CategoryDisabled)
        )
        newSplits

  let fn = _GroupSetCategorySplits asClientM trace gid setSplitObjs
  setSplitRes <- liftAndCatchIO $ runClientM fn accountsEnv
  case setSplitRes of
    Right NoContent -> return ()
    Left  e         -> do
      liftAndCatchIO
        $  putStr "Error: setCategorySplits _GroupSetCategorySplits "
        >> print (uid, gid, newSplits, e)
      status status500 >> Scotty.json (object ["success" .= False])

  Scotty.json (object ["success" .= True])

getCategorySplits :: V.Key SessionData -> ClientEnv -> ActionM ()
getCategorySplits sKey accountsEnv = do
  gid :: GroupId <- GroupId . fromJust . fromText <$> param "id"

  -- nil GroupId is used in demo mode
  when (gid == GroupId nil) $ do
    Scotty.json ([] :: [NewCategorySplits])
    finish

  -- continue if not in demo mode

  uid <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace <- createTrace

  groupE <- liftAndCatchIO
    $ runClientM (_GroupGet asClientM trace gid) accountsEnv
  groupModel <- case groupE of
    Left e ->
      status status404
        >> Scotty.json (object [("error", "group not found"), "msg" .= show e])
        >> finish
    Right g -> return g

  let members  = mbrUser <$> grpMembers groupModel
  let isMember = uid `elem` members

  unless
    isMember
    (status status403 >> Scotty.json (object [("error", "NotMember")]) >> finish
    )

  let fn = _GroupGetCategorySplits asClientM trace gid
  getSplitRes <- liftAndCatchIO $ runClientM fn accountsEnv
  case getSplitRes of
    Left e -> do
      liftAndCatchIO
        $  putStr "Error: setCategorySplits _GroupSetCategorySplits "
        >> print (uid, gid, e)
      status status500 >> Scotty.json (object ["success" .= False])
    Right catSplits -> do
      let
        alfieObj = fmap
          (\CategorySplit {..} -> NewCategorySplits
            { catcode       = categoryId
            , catEnabled    = case state of
                                CategoryActive   -> True
                                CategoryDisabled -> False
            , thisUserSplit =
              head
              . fmap
                  (\GroupSplit {..} -> ceiling (fromRational splRatio :: Double)
                  )
              . filter (\GroupSplit {..} -> splUser == uid)
              $ splits
            }
          )
          catSplits
      Scotty.json alfieObj

