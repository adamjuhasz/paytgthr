module AFSM.User.Query.Password where

import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           Shared.Models.User             ( EmailAddress
                                                , Password
                                                , UserID
                                                , UserModel(..)
                                                , normalizeEmail
                                                )

data UserPassword
  = UserNotExist
  | PasswordEmpty UserID
  | PasswordIs UserID Password
  deriving (Eq, Show)

getPasswordForEmail :: HasGetUserDB m => EmailAddress -> m UserPassword
getPasswordForEmail email = do
  user <- getUserByEmail $ normalizeEmail email
  return $ case user of
    Nothing -> UserNotExist
    Just UserModel { usrUserID = uid, usrPassword = Nothing } ->
      PasswordEmpty uid
    Just UserModel { usrUserID = uid, usrPassword = Just m } ->
      PasswordIs uid m
