module AFSM.User.Create
  ( createUser
  ) where

import           AFSM.FSM.User                  ( UserEvent
                                                , createAccount
                                                , increaseUserRevision
                                                , sendStateChangeEvents
                                                , setUserState
                                                , unShadowAccount
                                                , updateAccount
                                                )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(..) )
import           Data.Function                  ( (&) )
import           Data.Text                      ( Text )
import           Shared.Models.User             ( EmailAddress
                                                , Password
                                                , UserID
                                                , UserModel(..)
                                                , UserState(UserCreated)
                                                , UserTrait(..)
                                                , normalizeEmail
                                                )
import           Shared.TgthrMessages.Base      ( MessageID )

type FirstName = Text

createUser
  :: (HasGetUserDB m, GetCurrentTime m, HasSaveUserDB m)
  => MessageID
  -> EmailAddress
  -> Maybe Password
  -> Maybe FirstName
  -> UserID
  -> m (UserID, [UserEvent])
createUser mid inEmail inPassword firstName idempotentUID = do
  let email = normalizeEmail inEmail

  doesUserExist <- getUserByEmail email
  now           <- getCurrentTime

  let
    (evts, model) = case doesUserExist of
      -- totally new user
      Nothing ->
        createAccount now mid idempotentUID email inPassword
          & updateAccount mid [(NameFirst, firstName)]
      -- user has a shadow account, fill it up
      Just user@UserModel { usrUserState = UserCreated } ->
        user
          & unShadowAccount now mid inPassword
          & setUserState
          & sendStateChangeEvents user
          & increaseUserRevision mid
      -- user is past sign up phase
      Just u -> error $ "user is not in correct state for CreateUser " <> show
        (usrUserID u, inEmail, firstName, idempotentUID, u)

  saveUserModel model
  return (usrUserID model, evts)
