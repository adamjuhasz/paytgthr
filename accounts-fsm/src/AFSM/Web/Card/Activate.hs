{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module AFSM.Web.Card.Activate where

import           AFSM.AppMonad                  ( CanProcessUserEvents )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(getCard) )
import           AFSM.User.Change.Card          ( CardUpdate(..)
                                                , ChangeCardStateError
                                                  ( IllegalStateChange
                                                  )
                                                , changeCardState
                                                )
import           AFSM.Web.Event.ProcessUserEvents
                                                ( processUserEvent )
import           Control.Monad                  ( when )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Servant                        ( NoContent(..)
                                                , ServerError(errBody)
                                                , err403
                                                , err500
                                                )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Card            as Card
                                                ( CardLastFour(CardLastFour)
                                                , CardModel
                                                  ( cardLastFour
                                                  , cardPlatform
                                                  )
                                                , CardStatus(CardActive)
                                                )
import           Shared.Models.Ids              ( CardId
                                                , UserID
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )
import           Shared.WebAPI.General.Issuer   ( CardActivateBody(..) )

activateCard
  :: (CanProcessUserEvents m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> CardId
  -> CardActivateBody
  -> m NoContent
activateCard trace uid cid CardActivateBody {..} = do
  cardMaybe <- getCard trace cid
  card      <- case cardMaybe of
    Nothing -> do
      traceError trace "Error: card for ActivateCardBody not found " (uid, cid)
      throwError err500 { errBody = "card not found" }
    Just cm -> return cm

  let actualLastFour = CardLastFour (Card.cardLastFour card)
  when (cardLastFour /= actualLastFour)
       (throwError err403 { errBody = "Bad last four" })

  let platformId = cardPlatform card
  result <- changeCardState uid trace platformId $ CardAdminChange CardActive

  case result of
    Left (IllegalStateChange e) -> do
      traceError trace
                 "Error: activateCard "
                 (uid, cid, cardLastFour, Card.cardLastFour card, e)
      throwError err403 { errBody = "Bad state change" }
    Right evts -> do
      tracePrint trace
                 "activateCard success "
                 (uid, cid, cardLastFour, Card.cardLastFour card)
      mapM_ (processUserEvent trace) evts

  return NoContent
