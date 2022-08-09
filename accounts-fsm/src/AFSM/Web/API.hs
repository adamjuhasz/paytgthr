{-# LANGUAGE DataKinds, FlexibleContexts #-}

module AFSM.Web.API
  ( Routes
  , genServerHandler
  ) where

import           AFSM.AppMonad                  ( AppConfig(isShuttingDown)
                                                , CanProcessGroupEvents
                                                , CanProcessUserEvents
                                                )
import           AFSM.DB.Tokens                 ( HasTokenDB )
import           AFSM.Monad.HasReferralDB       ( HasReferralDB )
import           AFSM.Web.Admin.RecordUserNote  ( recordUserNote )
import           AFSM.Web.Admin.SyncSegment     ( syncSpecifcUser
                                                , syncToSegment
                                                )
import           AFSM.Web.Card.Activate         ( activateCard )
import           AFSM.Web.Card.Query            ( findACard
                                                , getACard
                                                )
import           AFSM.Web.Card.SetPin           ( setCardPin )
import           AFSM.Web.Group.Change.AcceptSplit
                                                ( acceptSplit )
import           AFSM.Web.Group.Change.ApproveMembership
                                                ( approveGroupMembership )
import           AFSM.Web.Group.Change.CloseGroup
                                                ( closeGroup )
import           AFSM.Web.Group.Change.CreateGroup
                                                ( createGroup )
import           AFSM.Web.Group.Change.DeclineMembership
                                                ( declineGroupMembership )
import           AFSM.Web.Group.Change.ForceSplit
                                                ( forceSplit )
import           AFSM.Web.Group.Change.GroupSplit
                                                ( changeSplit
                                                , setCatSplit
                                                )
import           AFSM.Web.Group.Query.ForUser   ( getUsersGroups )
import           AFSM.Web.Group.Query.Get       ( getGroupByID )
import           AFSM.Web.Group.Query.Groupsplit
                                                ( getGroupCatSplits )
import           AFSM.Web.Referral.Referral     ( accessReferralProgram
                                                , getReferralLinks
                                                , getReferralProgress
                                                , getRererralCode
                                                , setReferralProgram
                                                , setReferralProgress
                                                , updateReferralProgress
                                                , useReferralCode
                                                )
import           AFSM.Web.Rewards.Rewards       ( activateRewardForGroup
                                                , createNewRewardBoost
                                                , getActiveRewards
                                                , getGroupsActiveRewards
                                                , getHistoricalRewards
                                                , getSpecific
                                                , markAsUsed
                                                , rewardsCancelSpecific
                                                , updateReward
                                                )
import           AFSM.Web.Token.CreateToken     ( createToken )
import           AFSM.Web.Token.VerifyToken     ( verifyToken )
import           AFSM.Web.User.Change.Consent   ( acceptConsent )
import           AFSM.Web.User.Change.Create    ( createUser )
import           AFSM.Web.User.Change.CreateCardholder
                                                ( sendToIssuer )
import           AFSM.Web.User.Change.Disclosure
                                                ( acceptDisclosure )
import           AFSM.Web.User.Change.Email     ( changeEmail
                                                , verifyEmail
                                                )
import           AFSM.Web.User.Change.FundingSource
                                                ( createNewFundingSource
                                                , removeFundingSource
                                                , verifyCurrentFS
                                                )
import           AFSM.Web.User.Change.Identify  ( identifyCellCarrier
                                                , identifyDevice
                                                , identifyIPAddress
                                                )
import           AFSM.Web.User.Change.Invite    ( acceptInvite
                                                , createInvite
                                                , getInvite
                                                )
import           AFSM.Web.User.Change.IssueCard ( issueACard )
import           AFSM.Web.User.Change.KYC       ( runCIPProgram )
import           AFSM.Web.User.Change.Password  ( changePassword )
import           AFSM.Web.User.Change.Phone     ( verifyPhone )
import           AFSM.Web.User.Change.SendToDwolla
                                                ( sendToDwolla )
import           AFSM.Web.User.Change.State     ( adminSetCardState
                                                , adminlockAllCards
                                                , changeUserState
                                                , closeUser
                                                , setCardState
                                                , setKYCState
                                                )
import           AFSM.Web.User.Change.Update    ( updateUser )
import           AFSM.Web.User.Query.Get        ( getCardsForUser
                                                , getUserByID
                                                )
import           AFSM.Web.User.Query.GetUsersByState
                                                ( getActiveUsers )
import           AFSM.Web.User.Query.GetUsersByTrait
                                                ( getUserByBankNumbers
                                                , getUserByCardId
                                                , getUserByEmailAddress
                                                , getUserByEncryptedSSN
                                                , getUserByPhoneNumber
                                                , getUsersByCardholder
                                                )
import           AFSM.Web.User.Query.Password   ( getPasswordForUser )
import           Control.Concurrent             ( tryReadMVar )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Data.Text                      ( Text )
import           Servant                        ( ServerError
                                                , err503
                                                )
import           Servant.Server.Generic         ( AsServerT )
import           Shared.Track.HasTracing        ( HasTracing )
import           Shared.WebAPI.AccountsFSM.API  ( Routes(..) )

genServerHandler
  :: ( MonadError ServerError m
     , HasTokenDB m
     , CanProcessGroupEvents m
     , CanProcessUserEvents m
     , HasTracing m
     , HasReferralDB m
     , MonadReader AppConfig m
     )
  => Routes (AsServerT m)
genServerHandler = Routes {
-- /user/:uid
                            _UserGet                = getUserByID
                          , _UserCreate             = createUser
                          , _UserUpdate             = updateUser
                          , _UserAcceptDisclosure   = acceptDisclosure
                          , _UserAcceptConsent      = acceptConsent
                          , _UserSetPassword        = changePassword
                          , _UserCreateFS           = createNewFundingSource
                          , _UserVerifyCurrentFS    = verifyCurrentFS
                          , _UserRemoveFS           = removeFundingSource
                          , _UserChangeEmail        = changeEmail
                          , _UserVerifyEmail        = verifyEmail
                          , _UserForceState         = changeUserState
                          , _UserClose              = closeUser
                          , _UserRunKYC             = runCIPProgram
                          , _UserSetKYCState        = setKYCState
                          , _UserSetCardState       = setCardState
                          , _UserSetCardPin         = setCardPin
                          , _UserCreateCard         = issueACard
                          , _UserCardActivate       = activateCard
                          , _UserLockAllCards       = adminlockAllCards
                          , _UserSendToPrivacy      = sendToIssuer
                          , _UserSendToDwolla       = sendToDwolla
                          , _UserCardList           = getCardsForUser
                          , _UserVerifyPhone        = verifyPhone
                          , _UserIdentifyCarrier    = identifyCellCarrier
                          , _UserIdentifyDeviceIP   = identifyIPAddress
                          , _UserIdentifyDevice     = identifyDevice
                          , _UserMakeInvite         = createInvite
                          , _UserGetInvite          = getInvite
                          , _UserAcceptInvite       = acceptInvite

  -- /user/query
                          , _UserGetPassword        = getPasswordForUser

  -- /users/
                          , _UsersQueryAllActive    = getActiveUsers
                          , _UsersQueryCardholder   = getUsersByCardholder
                          , _UsersQueryCard         = getUserByCardId
                          , _UsersQueryPhone        = getUserByPhoneNumber
                          , _UsersQueryEmail        = getUserByEmailAddress
                          , _UsersQueryBank         = getUserByBankNumbers
                          , _UsersQuerySSN          = getUserByEncryptedSSN

  -- /group/:gid
                          , _GroupGet               = getGroupByID
                          , _GroupCreate            = createGroup
                          , _GroupClose             = closeGroup
                          , _GroupApproveInvite     = approveGroupMembership
                          , _GroupDeclineInvite     = declineGroupMembership
                          , _GroupChangeSplit       = changeSplit
                          , _GroupApproveSplit      = acceptSplit
                          , _GroupForceSplit        = forceSplit
                          , _GroupSetCategorySplits = setCatSplit
                          , _GroupGetCategorySplits = getGroupCatSplits
                          , _GroupActivateReward    = activateRewardForGroup
                          , _GroupGetRewards        = getGroupsActiveRewards
                          , _GroupMarkRewardAsUsed  = markAsUsed

  -- /groups/
                          , _GroupsForUser          = getUsersGroups

  -- /rewards
                          , _RewardsGetList         = getActiveRewards
                          , _RewardsGetSpecific     = getSpecific
                          , _RewardsAddNew          = createNewRewardBoost
                          , _RewardsGetAllEver      = getHistoricalRewards
                          , _RewardsEditSpecific    = updateReward
                          , _RewardsCancelSpecific  = rewardsCancelSpecific

  -- /referral
                          , _GetReferralProgram     = accessReferralProgram
                          , _SetReferralProgram     = setReferralProgram
                          , _GetRererralCode        = getRererralCode
                          , _UseReferralCode        = useReferralCode
                          , _GetReferralLinks       = getReferralLinks
                          , _GetReferralProgress    = getReferralProgress
                          , _SetReferralProgress    = setReferralProgress
                          , _UpdateReferralProgress = updateReferralProgress
  -- /token/:uid
                          , _TokenCreate            = createToken
                          , _TokenVerify            = verifyToken

  -- /card/
                          , _GetCard                = getACard
                          , _FindCard               = findACard
  -- system
                          , _health                 = healthHandler
                          , _AdminSyncSegment       = syncToSegment
                          , _AdminSyncUserSegment   = syncSpecifcUser
                          , _AdminSetCardState      = adminSetCardState
                          , _AdminRecordUserNote    = recordUserNote
                          }

healthHandler
  :: (MonadReader AppConfig m, MonadError ServerError m, MonadIO m) => m Text
healthHandler = do
  mvar <- asks isShuttingDown
  res  <- liftIO $ tryReadMVar mvar
  case res of
    Nothing    -> return "AccountsFSM"
    Just False -> return "AccountsFSM"
    Just True  -> do
      liftIO $ putStrLn "SIGTERM received... returning 503 Internal"
      throwError err503
