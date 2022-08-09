module APIApto.Card.Close where

import           APIApto.Monad.Accounts         ( HasAccounts(getUser) )
import           APIApto.Monad.Apto            as Apto
                                                ( HasAptoClient(closeCard) )
import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Shared.Models.User             ( UserID
                                                , UserModel(usrAptoCardId)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

data CloseCardErrors
  = UserMissingCard
  | HTTPFailure
  deriving (Eq, Show)
instance Exception CloseCardErrors

closeCard
  :: (HasAccounts m, HasAptoClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> m ()
closeCard trace userID = do
  let messageID = traceToMID trace
  liftIO $ putStr "closeCard " >> print (userID, trace)
  user <- getUser trace userID

  case user >>= usrAptoCardId of
    Nothing     -> return ()
    Just cardId -> do
      res <- Apto.closeCard messageID cardId
      case res of
        Left e -> do
          liftIO $ putStr "Error: closeCard card closing failed " >> print
            (userID, trace, e)
          throw HTTPFailure
        Right cardDetails -> do
          liftIO $ putStr "closeCard success" >> print
            (userID, trace, cardDetails)
          return ()
