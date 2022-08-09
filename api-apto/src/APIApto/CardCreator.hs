module APIApto.CardCreator where

import           APIApto.Apto.Actions
import           Data.Maybe
import           Shared.Models.Group
import           Shared.Models.User

createCardsIfNeeeded
  :: ([UserModel] -> IO (Either ActionError [CardTuple]))
  -> GroupModel
  -> [UserModel]
  -> IO [CardTuple]
createCardsIfNeeeded createCards group users = do
  -- verify group is active
  let groupState = grpStatus group

  -- filter users are active & needing cards
  let activeUsers     = filter isActive users
      hasCardholderId = filter hasCardholdr activeUsers
      withoutCards    = filter hasNoCard hasCardholderId
      allUsersReady   = length users == length hasCardholderId

  -- debug
  putStr
      "createCardsIfNeeeded: (group, users, activeUsers, hasCardholderId, withoutCards, allUsersReady)"
    >> print
         ( group
         , users
         , activeUsers
         , hasCardholderId
         , withoutCards
         , allUsersReady
         )
  putStr "createCardsIfNeeeded: (groupState, allUsersReady, withoutCards)"
    >> print (groupState, allUsersReady, withoutCards)

    -- make cards
  result <- createCards withoutCards

  case result of
    Left  _     -> return []
    Right cards -> return cards
 where
  isActive     = (UserActive ==) . usrUserState
  hasNoCard    = isNothing . usrAptoCardId
  hasCardholdr = isJust . usrAptoCardholderID

