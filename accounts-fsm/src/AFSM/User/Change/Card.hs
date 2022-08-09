{-# LANGUAGE RecordWildCards #-}

module AFSM.User.Change.Card
  ( changeCardState
  , CardUpdate(..)
  , ChangeCardStateError(..)
  ) where

import           AFSM.FSM.User                  ( UserEvent(..)
                                                , increaseUserRevision
                                                , sendStateChangeEvents
                                                , setUserDates
                                                , setUserState
                                                , updateAptoCardState
                                                )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(..) )
import           AFSM.User.Tools.Diff           ( diffUser )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import           GHC.Stack                      ( HasCallStack )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Card            as Card
                                                ( AptoCardId(..)
                                                , CardModel(..)
                                                , CardStatus(..)
                                                , IssuerPlatform(..)
                                                , PrivacyCardToken(..)
                                                )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.AccountsFSM.API as API
                                                ( ChangeCardStateBody
                                                  ( cardMemo
                                                  , cardState
                                                  )
                                                , traceToMID
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

data CardUpdate
  = CardStateUpdate CardStatus
  | CardUpdate ChangeCardStateBody
  | CardAdminChange CardStatus
  deriving (Eq, Show)

newtype ChangeCardStateError = IllegalStateChange Text
  deriving (Eq, Show)

changeCardState
  :: ( HasCallStack
     , HasGetUserDB m
     , HasSaveUserDB m
     , MonadIO m
     , GetCurrentTime m
     )
  => UserID
  -> TraceContext
  -> IssuerPlatform
  -> CardUpdate
  -> m (Either ChangeCardStateError [UserEvent])
changeCardState user trace card updates = do
  tracePrint trace "changeCardState " (user, card, updates)

  now         <- getCurrentTime
  currentUser <- getUserById user <&> fromJust

  let cardSrcId = case card of
        AptoPayments   aptoCardId             -> aptoCardId
        PayWithPrivacy (PrivacyCardToken cid) -> AptoCardId cid

  let newCardState = case updates of
        CardStateUpdate s  -> s
        CardUpdate      us -> cardState us
        CardAdminChange s  -> s

  let newMemo = case updates of
        CardStateUpdate _  -> Nothing
        CardUpdate      us -> API.cardMemo us
        CardAdminChange _  -> Nothing

  -- no side effects
  cards <-
    filter (\CardModel {..} -> cardPlatform == card) <$> getCardsFor trace user

  -- no side effects
  currentCard <- case cards of
    firstCard : _ -> return firstCard
    []            -> do
      traceError trace "Error: no card found " (user, card, updates)
      error $ "Error: no card found " <> show (user, card, updates, trace)

  let mid = traceToMID trace
  let (evts, newUser) =
        currentUser
          & updateAptoCardState now cardSrcId newCardState
          & setUserState
          & setUserDates now currentUser
          & sendStateChangeEvents currentUser
          & increaseUserRevision mid

  let newCardDated = case newCardState of
        CardCreated     -> currentCard
        CardActive      -> currentCard { activatedAt = Just now }
        CardUserFrozen  -> currentCard
        CardAdminFrozen -> currentCard
        CardClosed      -> currentCard { closedAt = Just now }

  let newCard = newCardDated
        { cardRevision  = cardRevision currentCard + 1
        , updatedAt     = now
        , cardStatus    = newCardState
        , Card.cardMemo = case (Card.cardMemo currentCard, newMemo) of
                            (orig, Nothing) -> orig
                            (_   , m      ) -> m
        }

  let cardEvts =
        [ EventUserCardStateChangedFromTo (cardStatus currentCard) newCard
        | cardStatus currentCard /= cardStatus newCard
        ]

  -- Check permissions to see if we should allow this update
  let stateChangeOK =
        allowCardStateChange (cardStatus currentCard) newCardState updates

  case stateChangeOK of
    Left  reason -> return $ Left $ IllegalStateChange reason
    Right _      -> do
      saveUserModel newUser
      saveCardModel trace newCard

      -- print debug diff
      diffUser trace currentUser newUser

      tracePrint trace
                 "changeCardState done "
                 (user, card, updates, newCard, evts)

      return $ Right $ evts <> cardEvts

type CurrentStats = CardStatus
type NewStatus = CardStatus
-- inline brittany config for width
-- brittany-next-binding --columns 500
allowCardStateChange :: CurrentStats -> NewStatus -> CardUpdate -> Either Text ()
allowCardStateChange _               CardClosed      _                 = Right ()
allowCardStateChange CardClosed      _               _                 = Left "Cannot change a CardClosed card"
allowCardStateChange CardCreated     CardCreated     _                 = Right ()
allowCardStateChange CardCreated     CardActive      CardAdminChange{} = Right ()
allowCardStateChange CardCreated     CardActive      _                 = Left "Must use `CardActivation` to activate"
allowCardStateChange CardCreated     CardUserFrozen  _                 = Left "Must use `CardActivation` to activate"
allowCardStateChange _               CardCreated     _                 = Left "Cannot change bact to CardCreated"
allowCardStateChange CardAdminFrozen _               CardAdminChange{} = Right ()
allowCardStateChange CardAdminFrozen _               _                 = Left "CardAdminFrozen can't be changed"
allowCardStateChange _               CardActive      _                 = Right ()
allowCardStateChange _               CardUserFrozen  _                 = Right ()
allowCardStateChange _               CardAdminFrozen _                 = Right ()
