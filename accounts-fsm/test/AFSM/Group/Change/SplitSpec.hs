{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{- HLINT ignore "Redundant do" -}

module AFSM.Group.Change.SplitSpec where

import           AFSM.AppMonad                  ( AppIOM(unAppIOM) )
import           AFSM.DB.DBActions              ( DBActions(..) )
import           AFSM.FSM.Group                 ( GroupEvent(..) )
import           AFSM.Group.Change.Split        ( approveGroupSplit
                                                , changeGroupSplit
                                                , forceSplitChange
                                                , setCategorySplits
                                                )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasGetGroupDB       ( HasGetGroupDB(..) )
import           AFSM.Monad.HasSaveGroupDB      ( HasSaveGroupDB(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Data.Maybe                     ( fromJust )
import qualified Data.Time.Clock               as Clock
import           Data.UUID                      ( fromText
                                                , nil
                                                )
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import           Shared.Models.CategorySplit    ( CategoryCode(..)
                                                , CategorySplit(..)
                                                , CategoryState(..)
                                                )
import           Shared.Models.Group            ( GroupId(GroupId)
                                                , GroupMember(GroupMember)
                                                , GroupModel(..)
                                                , GroupSplit(..)
                                                , GroupStatus(..)
                                                )
import           Shared.Models.User             ( UserID(UserID) )
import           Shared.TgthrMessages.Base      ( MessageID(MessageID) )
import           Shared.WebAPI.General.API      ( TraceContext(TraceContext)
                                                , randomTrace
                                                )
import           Test.AppConf                   ( appConf )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

theGroupId :: GroupId
theGroupId = GroupId nil

user1 :: UserID
user1 = (UserID . fromJust . fromText) "00000000-0000-0000-0000-000000000000"

user2 :: UserID
user2 = (UserID . fromJust . fromText) "00000000-0000-0000-0000-000000000000"

group :: GroupModel
group = GroupModel
  { grpId        = theGroupId
  , grpStatus    = GroupPending
  , grpStart     = Nothing
  , grpEnd       = Nothing
  , grpSplit     = [GroupSplit user1 50 True, GroupSplit user2 50 False]
  , grpMembers   = [GroupMember user1 True, GroupMember user2 True]
  , grpRevision  = 1
  , grpVersion   = "1.0"
  , grpMsgSource = MessageID nil
  , grpCreatedAt = unsafePerformIO Clock.getCurrentTime
  }

newtype AppTestM a =
  AppTestM { unAppTestM :: IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )
instance GetCurrentTime AppTestM where
  getCurrentTime = liftIO Clock.getCurrentTime
instance HasGetGroupDB AppTestM where
  getGroupByGroupId _ = return $ Just group
  getGroupSplits _ = do
    now <- getCurrentTime
    return
      [ CategorySplit
          { groupId     = theGroupId
          , categoryId  = Category000
          , catRevision = 1
          , createdAt   = now
          , updatedAt   = now
          , splits      = [GroupSplit user1 50 True, GroupSplit user2 50 True]
          , state       = CategoryActive
          }
      ]
instance HasSaveGroupDB AppTestM where
  saveGroupModel _ = return ()
  saveCategorySplit CategorySplit { categoryId = Category000, catRevision = 2 }
    = return ()
  saveCategorySplit CategorySplit { categoryId = Category001, catRevision = 1 }
    = return ()
  saveCategorySplit c = error $ "bad cat save" <> show c

spec :: Spec
spec = do
  let dbActionsRcrd = DBActions
        { cSaveUser                  = \_ -> error "mocked cSaveUser"
        , cGetAccountByEmail         = \_ -> error "mocked"
        , cGetAccountByPhone         = \_ -> error "mocked"
        , cGetAccountById            = \_ -> error "mocked cGetAccountById"
        , cGetAccounByCardholder     = \_ -> error "mocked"
        , cSaveGroup                 = \_ -> error "mocked"
        , cGetGroupById              = \_ -> error "mocked"
        , cGetGroupsByUserId         = \_ -> error "mocked"
        , cGetGroupsByUserIdFiltered = \_ -> error "mocked"
        , cGetAllActiveUsers         = return []
        , cGetUsersWithAcountNumber  = \_ -> error "mocked"
        , cGetUsersWithSSN           = \_ -> error "mocked"
        , cFindToken                 = \_ -> error "mocked"
        , cSaveToken                 = \_ -> error "mocked"
        }

  describe "changeGroupSplit" $ do
    describe "Changes splits" $ do
      it "happy path" $ do
        trace <- randomTrace
        let cGetGroupById u | u == grpId group = return $ Just group
                            | otherwise        = return Nothing
        let cSaveGroup _ = return ()
        let fn = changeGroupSplit
              trace
              theGroupId
              user1
              [GroupSplit user1 90 True, GroupSplit user2 10 False]
        res <- runReaderT
          (unAppIOM fn)
          (appConf $ dbActionsRcrd { cGetGroupById, cSaveGroup })
        res
          `shouldBe` [ EventGroupSplitChanged (GroupId nil) user1 [user1, user2]
                     , EventGroupStateChanged (GroupId nil)
                                              GroupActive
                                              user1
                                              [user1, user2]
                     ]

    describe "Approves splits" $ do
      it "happy path" $ do
        let cGetGroupById u | u == grpId group = return $ Just group
                            | otherwise        = return Nothing
        let cSaveGroup _ = return ()
        trace <- randomTrace
        let fn = approveGroupSplit trace theGroupId user1
        res <- runReaderT
          (unAppIOM fn)
          (appConf $ dbActionsRcrd { cGetGroupById, cSaveGroup })
        res
          `shouldBe` [ EventGroupStateChanged (GroupId nil)
                                              GroupActive
                                              user1
                                              [user1, user2]
                     ]

    describe "Force Changes splits" $ do
      it "happy path" $ do
        let cGetGroupById u | u == grpId group = return $ Just group
                            | otherwise        = return Nothing
        let cSaveGroup _ = return ()
        trace <- randomTrace
        let fn = forceSplitChange
              trace
              theGroupId
              [GroupSplit user1 90 True, GroupSplit user2 10 False]
        res <- runReaderT
          (unAppIOM fn)
          (appConf $ dbActionsRcrd { cGetGroupById, cSaveGroup })
        res
          `shouldBe` [ EventGroupSplitChanged (GroupId nil) user1 [user1, user2]
                     , EventGroupStateChanged (GroupId nil)
                                              GroupActive
                                              user1
                                              [user1, user2]
                     ]

  describe "setCategorySplits" $ do
    let trace = TraceContext "6158c2f033333333333346d2e4c02e42" 1
    it "happy path" $ do
      let fn = setCategorySplits
            trace
            theGroupId
            Category001
            [GroupSplit user1 50 True, GroupSplit user2 50 True]
            CategoryActive
      res <- unAppTestM fn
      res `shouldBe` []

    it "modifies existing" $ do
      let fn = setCategorySplits
            trace
            theGroupId
            Category000
            [GroupSplit user1 60 True, GroupSplit user2 40 True]
            CategoryActive
      res <- unAppTestM fn
      res
        `shouldBe` [ EventGroupSplitChanged theGroupId user1 [user1, user2]
                   , EventGroupStateChanged theGroupId
                                            GroupActive
                                            user1
                                            [user1, user2]
                   ]
