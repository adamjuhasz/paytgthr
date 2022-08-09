module AFSM.Web.Card.Query where

import           AFSM.Monad.HasGetUserDB                ( HasGetUserDB(..) )
import           Shared.Models.Card             ( CardId
                                                , CardModel
                                                , IssuerPlatform
                                                )
import           Shared.WebAPI.AccountsFSM.API  ( TraceContext )

getACard :: (HasGetUserDB m) => TraceContext -> CardId -> m (Maybe CardModel)
getACard = getCard

findACard
  :: (HasGetUserDB m) => TraceContext -> IssuerPlatform -> m (Maybe CardModel)
findACard = findCard
