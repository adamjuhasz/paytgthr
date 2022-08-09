module AFSM.Monad.HasGetUserDB where

import           Data.Text                      ( Text )
import           Shared.Models.Card             ( CardId(..)
                                                , CardModel
                                                , IssuerPlatform(..)
                                                )
import           Shared.Models.Cardholder       ( CardholderId(..) )
import           Shared.Models.KYCAssesment     ( KYCAssesment )
import           Shared.Models.User             ( EmailAddress(..)
                                                , PhoneNumber(..)
                                                , RedactedText(..)
                                                , UserID(..)
                                                , UserModel
                                                , UserState(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

type RoutingNumber = RedactedText
type AccountNumber = RedactedText
type Revision = Int

class Monad m => HasGetUserDB m where
  getUserByEmail         :: EmailAddress -> m (Maybe UserModel)
  getUserByPhone         :: PhoneNumber -> m (Maybe UserModel)
  getUserById            :: UserID -> m (Maybe UserModel)
  getUserByCardholder    :: CardholderId -> m (Maybe UserModel)
  getUserByCard          :: TraceContext -> IssuerPlatform -> m (Maybe UserModel)
  getUserByBank          :: RoutingNumber -> AccountNumber -> m [UserID]
  getAllActiveUsers      :: m [UserID]
  getUsersWithSSN        :: Text -> m [UserID]
  getCardsFor            :: TraceContext -> UserID -> m [CardModel]
  getCard                :: TraceContext -> CardId -> m (Maybe CardModel)
  findCard               :: TraceContext -> IssuerPlatform -> m (Maybe CardModel)
  getAllUsers            :: [UserState] -> m [UserID]
  getUsersKYCAssessments :: TraceContext -> UserID -> m [KYCAssesment]
  getUserNote            :: TraceContext -> UserID -> m (Maybe (Text, Revision))
