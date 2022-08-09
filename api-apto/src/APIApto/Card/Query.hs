{-# LANGUAGE RecordWildCards #-}

module APIApto.Card.Query where

import           APIApto.Monad.Accounts         ( HasAccounts(getUser) )
import           APIApto.Monad.Apto            as Apto
                                                ( HasAptoClient(getCard) )
import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Shared.Models.Apto.Card        ( AptoCard(..)
                                                , CardLastFour
                                                )
import           Shared.Models.User             ( UserID
                                                , UserModel(usrAptoCardId)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

data GetLastFourErrors
  = UserDoesntHaveCard
  | HttpFailure
  deriving (Eq, Show)
instance Exception GetLastFourErrors

getCardLastFour
  :: (HasAccounts m, HasAptoClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> m CardLastFour
getCardLastFour trace userID = do
  let messageID = traceToMID trace
  user <- getUser trace userID
  case (user, user >>= usrAptoCardId) of
    (Nothing, _) -> do
      liftIO $ putStr "Error: getCardLastFour getUser Can't find user " >> print
        (userID, trace)
      throw UserDoesntHaveCard
    (_, Nothing) -> do
      liftIO $ putStr "Error: (GetLastFour) Can't find card for user " >> print
        (userID, trace, user)
      throw UserDoesntHaveCard
    (_, Just card) -> do
      result <- Apto.getCard messageID card
      case result of
        Left  _             -> throw HttpFailure
        Right AptoCard {..} -> return acdxLastFour

