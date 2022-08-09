module APIApto.Monad.Apto where

import           APIApto.Apto.Actions           ( CardTuple
                                                , ActionError
                                                )
import           Shared.Models.Apto.Base        ( AptoCardholderId )
import           Shared.Models.Apto.Card        ( CardLastFour
                                                , AptoCard
                                                )
import           Shared.Models.Apto.Cardholder  ( AptoCardholderResponse )
import           Shared.Models.Card             ( AptoCardId
                                                , CardPinEnc
                                                )
import           Shared.Models.User             ( UserModel )
import           Shared.TgthrMessages.Base      ( MessageID )

class Monad m => HasAptoClient m where
  activateCard :: MessageID -> AptoCardId -> CardLastFour -> m (Either ActionError AptoCard)
  changePin :: MessageID -> AptoCardId -> CardPinEnc -> m (Either ActionError ())
  createCard :: MessageID -> [UserModel] -> m (Either ActionError [CardTuple])
  closeCard :: MessageID -> AptoCardId -> m (Either ActionError AptoCard)
  getCard :: MessageID -> AptoCardId -> m (Either ActionError AptoCard)
  createCardholder :: MessageID -> UserModel -> m (Either ActionError AptoCardholderResponse)
  getCardholder :: MessageID -> AptoCardholderId -> m (Either ActionError AptoCardholderResponse)
