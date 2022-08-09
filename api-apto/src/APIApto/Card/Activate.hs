module APIApto.Card.Activate where

import           APIApto.Apto.Actions           ( ActionError(..) )
import           APIApto.Apto.Client            ( RequesterError )
import           APIApto.Monad.Accounts         ( HasAccounts(..) )
import           APIApto.Monad.Apto            as MA
                                                ( HasAptoClient(..) )
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

data ActivationErrors
  = UserHasNoCard
  | LastFourNotCorrect
  | HttpFailure RequesterError
  deriving (Show)
instance Exception ActivationErrors

activateCard
  :: (HasAccounts m, HasAptoClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> CardLastFour
  -> m AptoCard
activateCard trace userID userGivenLastFour = do
  let mid = traceToMID trace
  liftIO $ putStr "APIApto.Card.Activate activateCard " >> print (userID, trace)

  user <- getUser trace userID

  case user >>= usrAptoCardId of
    Nothing -> do
      liftIO
        $  putStr
             "Error: APIApto.Card.Activate activateCard, Can't find card for user "
        >> print (userID, trace, user)
      throw UserHasNoCard
    Just card -> do
      liftIO $ putStr "APIApto.Card.Activate activateCard done " >> print
        (user, card, trace)

      result <- MA.activateCard mid card userGivenLastFour

      case result of
        Left (LastFourIncorrect _) -> throw LastFourNotCorrect
        Left (RequestFailure    e) -> do
          liftIO $ putStr "Error: APIApto.Card.Activate activateCard: " >> print
            (trace, user, card, e, userGivenLastFour)
          throw $ HttpFailure e
        Right c -> return c

