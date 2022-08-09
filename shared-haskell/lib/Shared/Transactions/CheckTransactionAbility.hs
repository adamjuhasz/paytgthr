module Shared.Transactions.CheckTransactionAbility where

import           Data.Either                    ( lefts
                                                , rights
                                                )
import           Data.Time.Clock                ( getCurrentTime )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.Group            ( GroupMember(mbrUser)
                                                , GroupModel(grpMembers)
                                                )
import           Shared.Models.User             ( UserID
                                                , UserModel(..)
                                                )
import           Shared.Transactions.UserGroupConsistencyCheck
                                                ( GroupCheckFailure
                                                , GroupCheckStrictness
                                                  ( LooseGroupCheck
                                                  )
                                                , UserCheckFailure
                                                , checkGroupConsistency
                                                , checkUserConsistency
                                                )
import           Shared.WebAPI.AccountsFSM.Client

data CanDoTrx
  = EverythingOK
  | ErrorAccessingUser
  | GroupCantDoTrx GroupCheckFailure
  | UserCantDoTrx  UserCheckFailure
  | PartnerCantDoTrx UserCheckFailure
  deriving (Eq, Show)

canUserMakeTrx
  :: ClientEnv -> TraceContext -> UserID -> [GroupModel] -> IO CanDoTrx
canUserMakeTrx accountsEnv trace userID groups = do

  now <- getCurrentTime
  let groupOK = checkGroupConsistency LooseGroupCheck now groups
  case groupOK of
    Left  e     -> return $ GroupCantDoTrx e
    Right group -> do
      let groupMembers = mbrUser <$> grpMembers group
      let getUser u = runClientM (_UserGet asClientM trace u) accountsEnv

      usersEi <- mapM getUser groupMembers
      let users         = rights usersEi
      let usersNotFound = lefts usersEi

      let usersNotOK = lefts $ fmap
            (\x -> case checkUserConsistency x of
              Left  r -> Left (r, x)
              Right u -> Right u
            )
            users

      case (usersNotFound, usersNotOK) of
        ([]   , []) -> return EverythingOK
        (_ : _, _ ) -> return ErrorAccessingUser
        (_, (theReason, UserModel { usrUserID = uid }) : _) ->
          return $ if uid == userID
            then UserCantDoTrx theReason
            else PartnerCantDoTrx theReason



