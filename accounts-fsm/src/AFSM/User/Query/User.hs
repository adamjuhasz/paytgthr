module AFSM.User.Query.User where

import           AFSM.FSM.User                  ( ABARouting(..)
                                                , DDANumber(..)
                                                )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           Shared.Models.Apto.Base        ( AptoCardholderId(..) )
import           Shared.Models.Cardholder       ( CardholderId(AptoPaymentsCH) )
import           Shared.Models.User             ( EmailAddress
                                                , PhoneNumber
                                                , RedactedText(RedactedText)
                                                , UserID
                                                , UserModel
                                                , normalizeEmail
                                                )

getUserWithId :: HasGetUserDB m => UserID -> m (Maybe UserModel)
getUserWithId = getUserById

getUserWithCardholderId
  :: HasGetUserDB m => AptoCardholderId -> m (Maybe UserModel)
getUserWithCardholderId = getUserByCardholder . AptoPaymentsCH

getActiveUsers :: HasGetUserDB m => m [UserID]
getActiveUsers = getAllActiveUsers

getUserWithEmail :: HasGetUserDB m => EmailAddress -> m (Maybe UserModel)
getUserWithEmail e = getUserByEmail (normalizeEmail e)

getUserWithPhone :: HasGetUserDB m => PhoneNumber -> m (Maybe UserModel)
getUserWithPhone = getUserByPhone

getUsersWithBankAccount
  :: HasGetUserDB m => ABARouting -> DDANumber -> m [UserID]
getUsersWithBankAccount (ABARouting routing) (DDANumber account) =
  getUserByBank (RedactedText routing) (RedactedText account)
