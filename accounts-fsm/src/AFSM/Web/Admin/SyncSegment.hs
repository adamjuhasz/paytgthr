module AFSM.Web.Admin.SyncSegment where

import           AFSM.Monad.HasEventTracking    ( HasEventTracking
                                                  ( trackOneOffTrait
                                                  , trackUser
                                                  )
                                                )
import           AFSM.Monad.HasGetGroupDB       ( HasGetGroupDB(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson                     ( KeyValue((.=)) )
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( listToMaybe )
import           Servant.API                    ( NoContent(..) )
import           Shared.Models.Group            ( GroupMember(mbrUser)
                                                , GroupModel
                                                  ( GroupModel
                                                  , grpId
                                                  , grpMembers
                                                  , grpStatus
                                                  )
                                                , GroupStatus(..)
                                                )
import           Shared.Models.User             ( UserID
                                                , UserModel
                                                  ( UserModel
                                                  , usrBankVerified
                                                  )
                                                )

mapSnd :: (t -> b) -> [(a, t)] -> [(a, b)]
mapSnd f xys = [ (x, f y) | (x, y) <- xys ]

topGroupState :: [GroupModel] -> Maybe GroupModel
topGroupState = listToMaybe . sortOn grpStatus

syncGroupState
  :: (HasGetUserDB m, HasGetGroupDB m, HasEventTracking m, MonadIO m)
  => UserID
  -> m ()
syncGroupState uid = do
  groups <- getGroupsForUser uid

  let topGroupStatus = topGroupState groups
  liftIO $ putStr "syncGroupState group state: " >> print (uid, topGroupStatus)
  let groupStatus = grpStatus <$> topGroupStatus
  canPurchase <- case topGroupStatus of
    Nothing -> return False
    Just GroupModel { grpStatus = GroupActive, grpMembers = members } -> do
      allUsers <- mapM getUserById $ fmap mbrUser members
      return $ foldr
        (\aUser accum -> case aUser of
          Nothing -> accum && False
          Just UserModel { usrBankVerified = Nothing } -> accum && False
          Just UserModel { usrBankVerified = Just False } -> accum && False
          Just UserModel { usrBankVerified = Just True } -> accum && True
        )
        True
        allUsers
    Just GroupModel{} -> return False

  liftIO $ putStr "syncGroupState groupStatus/canPurchase sync " >> print
    (uid, grpId <$> topGroupStatus, groupStatus, canPurchase)

  trackOneOffTrait
    uid
    ["groupStatus" .= groupStatus, "canPurchaseExperimental" .= canPurchase]

  return ()

syncToSegment :: (MonadIO m) => m NoContent
syncToSegment = do
  liftIO $ putStrLn " Syncing users to Segment start"

  -- activeUsers <- getAllUsers [UserActive]
  -- liftIO $ putStr "Segment: Number of UserActive users to sync: " >> print
  --   (length activeUsers)
  -- mapM_ trackUser activeUsers

  -- liftIO $ putStrLn "UserActive synced"

  -- mapM_ syncGroupState activeUsers

  -- liftIO $ putStrLn "syncGroupState synced"

  -- closedUsers <- getAllUsers
  --   [ UserClosed FraudyUser
  --   , UserClosed ClosedByUser
  --   , UserClosed OverdueBalance
  --   , UserClosed DuplicateUser
  --   , UserClosed ForeignDeviceDuringSignup
  --   ]
  -- liftIO $ putStr "Segment: Number of UserClosed users to sync: " >> print
  --   (length closedUsers)
  -- mapM_ trackUser closedUsers

  -- waitingOnKYCUsers <- getAllUsers [UserWaitingOnKYC]
  -- liftIO $ putStr "Number of UserWaitingOnKYC users to sync: " >> print
  --   (length waitingOnKYCUsers)
  -- mapM_ trackUser waitingOnKYCUsers

  -- kycDelayUsers <- getAllUsers [UserKYCDelay]
  -- liftIO $ putStr "Number of UserKYCDelay users to sync: " >> print
  --   (length kycDelayUsers)
  -- mapM_ trackUser kycDelayUsers

  -- kycUpdatedDelayUsers <- getAllUsers [UserUpdatedKYCDelay]
  -- liftIO $ putStr "Number of UserUpdatedKYCDelay users to sync: " >> print
  --   (length kycUpdatedDelayUsers)
  -- mapM_ trackUser kycUpdatedDelayUsers

  -- updatedUsers <- getAllUsers [UserUpdated]
  -- liftIO $ putStr "Number of UserUpdated users to sync: " >> print
  --   (length updatedUsers)
  -- mapM_ trackUser updatedUsers

  -- liftIO $ putStrLn "Syncing users to Segment done"

  return NoContent

syncSpecifcUser
  :: (HasGetUserDB m, HasGetGroupDB m, HasEventTracking m, MonadIO m)
  => UserID
  -> m NoContent
syncSpecifcUser userId = do
  trackUser userId
  syncGroupState userId

  return NoContent
