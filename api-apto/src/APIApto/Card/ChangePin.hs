module APIApto.Card.ChangePin where

import           APIApto.Monad.Accounts         ( HasAccounts(..) )
import           APIApto.Monad.Apto             ( HasAptoClient(changePin) )
import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Shared.Models.Card             ( AptoCardId
                                                , CardPinEnc
                                                , CardStatus(..)
                                                )
import           Shared.Models.User             ( UserID
                                                , UserModel
                                                  ( usrAptoCardId
                                                  , usrAptoCardStatus
                                                  )
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

data ChangePinErrors
  = CardHasIncorrectState
  | UserHasNoCard
  | HTTPFailure
  deriving (Eq, Show)
instance Exception ChangePinErrors

changeCardPin
  :: (HasAccounts m, HasAptoClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> CardPinEnc
  -> m AptoCardId
changeCardPin trace userID encryptedPinCode = do
  let messageID = traceToMID trace
  liftIO $ putStr "(ChangeCardPin) started " >> print (userID, trace)

  user   <- getUser trace userID

  cardID <- case (user >>= usrAptoCardId, user >>= usrAptoCardStatus) of
    (Nothing  , _                   ) -> errorCardNotFound
    (_        , Nothing             ) -> errorCardNotActive
    (_        , Just CardCreated    ) -> errorCardNotActive
    (_        , Just CardUserFrozen ) -> errorCardNotActive
    (_        , Just CardAdminFrozen) -> errorCardNotActive
    (_        , Just CardClosed     ) -> errorCardNotActive
    (Just card, Just CardActive     ) -> return card

  result <- changePin messageID cardID encryptedPinCode

  case result of
    Left  _  -> errorGeneral
    Right () -> do
      liftIO $ putStr "(ChangeCardPin) Successfully changed pin " >> print
        (userID, trace, user, cardID)
      return cardID
 where
  errorCardNotActive = throw CardHasIncorrectState
  errorCardNotFound  = do
    liftIO $ putStr "Error: (ChangeCardPin) Can't find card for user " >> print
      (userID, trace)
    throw UserHasNoCard
  errorGeneral = do
    liftIO $ putStr "Error: (ChangeCardPin) Can't change pin " >> print
      (userID, trace)
    throw HTTPFailure
