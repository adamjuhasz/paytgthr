module APIApto.Card.Create where

import           APIApto.Apto.Actions           ( CardTuple )
import           APIApto.Monad.Accounts         ( HasAccounts(getUser) )
import           APIApto.Monad.Apto            as Apto
                                                ( HasAptoClient(createCard) )
import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Data.Maybe                     ( fromJust )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

data CardCreateErrors = HttpFailure
  deriving (Eq, Show)
instance Exception CardCreateErrors

createCard
  :: (HasAptoClient m, HasAccounts m, MonadIO m)
  => TraceContext
  -> UserID
  -> m [CardTuple]
createCard trace userID = do
  let messageID = traceToMID trace
  liftIO $ putStr "APIApto.Card.Create createCard " >> print (userID, trace)

  user <- fromJust <$> getUser trace userID

  res  <- Apto.createCard messageID [user]

  liftIO $ putStr "APIApto.Card.Create createCard res " >> print
    (userID, res, trace)

  case res of
    Right cards -> return cards
    Left  e     -> do
      liftIO $ putStr "Error: APIApto.Card.Create createCard " >> print
        (trace, userID, e)
      throw HttpFailure
