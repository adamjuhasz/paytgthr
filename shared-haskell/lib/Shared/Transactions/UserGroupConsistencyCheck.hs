{- HLINT ignore "Use lambda-case" -}

module Shared.Transactions.UserGroupConsistencyCheck where

import           Data.Time.Clock                ( UTCTime )
import           Shared.Groups.SortGroups       ( sortGroups )
import           Shared.Models.Group            ( GroupModel
                                                , groupIsActive
                                                , groupIsPermanent
                                                )
import           Shared.Models.KYC
import           Shared.Models.User             ( UserModel(..)
                                                , UserState(..)
                                                )

data GroupCheckFailure
  = NoActiveGroup
  | MultiplePermGroups
  deriving (Eq, Show)

data GroupCheckStrictness
  = StrictGroupCheck -- For Chewy / Customer Service
  | LooseGroupCheck  -- For allowing Trx
  deriving (Eq, Show)

checkGroupConsistency
  :: GroupCheckStrictness
  -> UTCTime
  -> [GroupModel]
  -> Either GroupCheckFailure GroupModel
checkGroupConsistency _ _ [] = Left NoActiveGroup
checkGroupConsistency StrictGroupCheck now groups =
  let permGroups =
        filter groupIsPermanent . filter (groupIsActive now) $ groups
  in  if length permGroups > 1
        then Left MultiplePermGroups
        else checkGroupConsistency LooseGroupCheck now groups
checkGroupConsistency LooseGroupCheck now groups =
  let filtered = filter (groupIsActive now) . sortGroups now $ groups
  in  case filtered of
        []      -> Left NoActiveGroup
        (h : _) -> Right h

data UserCheckFailure
  = UserIsNotActive
  | UserIsClosed
  | NoFundingSourceLinked
  | NoFundingSourceVerified
  | KYCNotPassed
  deriving (Eq, Show)

checkUserConsistency :: UserModel -> Either UserCheckFailure UserModel
checkUserConsistency UserModel { usrUserState = UserWaitingOnKYC } =
  Left UserIsNotActive
checkUserConsistency UserModel { usrUserState = UserClosed _ } =
  Left UserIsClosed
checkUserConsistency UserModel { usrBankVerified = Nothing } =
  Left NoFundingSourceLinked
checkUserConsistency UserModel { usrBankVerified = Just False } =
  Left NoFundingSourceVerified
checkUserConsistency UserModel { usrAptoKYCStatus = Just (AutoVerifyFailed _) }
  = Left KYCNotPassed
checkUserConsistency model@UserModel { usrUserState = UserActive, usrBankVerified = Just True, usrAptoKYCStatus = Just Passed }
  = Right model
checkUserConsistency _ = Left UserIsNotActive

