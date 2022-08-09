module PaymentAuth.Monad.Accounts
  ( module PaymentAuth.Monad.Accounts
  , UserCreateFSBody(..)
  , QueryBankFSBody(..)
  ) where

import           Shared.Models.Card             ( CardModel
                                                , IssuerPlatform
                                                )
import           Shared.Models.CategorySplit    ( CategorySplit )
import           Shared.Models.Group            ( GroupModel )
import           Shared.Models.Ids              ( GroupId
                                                , ReferralProgramID
                                                , RewardId
                                                , TransactionId
                                                , UserID
                                                )
import           Shared.Models.Payment          ( PaymentFailureCode
                                                , PaymentMethod
                                                )
import           Shared.Models.Referral.ReferralProgram
import           Shared.Models.Referral.ReferralProgress
import           Shared.Models.Rewards.Boost    ( RewardBoost )
import           Shared.Models.User             ( UserModel )
import           Shared.WebAPI.AccountsFSM.API  ( QueryBankFSBody(..)
                                                , UserCreateFSBody(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

class Monad m => HasAccounts m where
  getGroupFor            :: TraceContext -> UserID -> m (Maybe GroupModel)
  getUser                :: TraceContext -> UserID -> m (Maybe UserModel)
  removeFundingSource    :: TraceContext -> UserID -> PaymentMethod -> Maybe PaymentFailureCode -> m ()
  getAllActiveUsers      :: TraceContext -> m [UserID]
  getCategorySplits      :: TraceContext -> GroupId -> m [CategorySplit ]
  setBankDetails         :: TraceContext -> UserID -> UserCreateFSBody -> m ()
  queryUsersWithBank     :: TraceContext -> QueryBankFSBody -> m [UserModel]
  verifyCurrentFS        :: TraceContext -> UserID -> m ()
  adminlockCardsForUser  :: TraceContext -> UserID -> m ()
  getRewardsForGroup     :: TraceContext -> GroupId -> m [RewardBoost]
  getReward              :: TraceContext -> RewardId -> m RewardBoost
  findCard               :: TraceContext -> IssuerPlatform -> m (Maybe CardModel)
  markRewardAsUsed       :: TraceContext -> (GroupId, RewardId) -> TransactionId -> m ()
  getReferralProgress    :: TraceContext -> UserID -> m (Maybe ReferralProgress)
  getReferralProgram     :: TraceContext -> ReferralProgramID -> m ReferralProgram
  updateReferralProgress :: TraceContext -> UserID -> WorkFlowProgress -> m ()
