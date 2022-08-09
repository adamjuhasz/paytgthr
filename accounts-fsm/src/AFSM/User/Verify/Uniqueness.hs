module AFSM.User.Verify.Uniqueness
  ( verifySSNNotInUse
  ) where

import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           Data.Text                      ( Text )
import           Shared.Models.User             ( UserID )

verifySSNNotInUse :: HasGetUserDB m => [UserID] -> Text -> m Bool
verifySSNNotInUse excludeUsers ssn = do
  userList <- getUsersWithSSN ssn
  let withoutExcluded = filter (`notElem` excludeUsers) userList
  return $ null withoutExcluded
