module APIApto.Cardholder.Create where

import           APIApto.Monad.Accounts         ( HasAccounts(getUser) )
import           APIApto.Monad.Apto            as Apto
                                                ( HasAptoClient(..) )
import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Shared.Models.Apto.Base        ( AptoCardholderId )
import           Shared.Models.Apto.Cardholder  ( AptoCardholderResponse
                                                  ( AptoCardholderResponse
                                                  , accxId
                                                  )
                                                )
import           Shared.Models.Cardholder
import           Shared.Models.User             ( UserID
                                                , UserModel(usrAptoCardholderID)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )
import           Shared.WebAPI.General.Issuer   ( CreateCardholderAction(..) )

data CreateCardholderErrors
 = MissingUser
 | HtttpFailure
 deriving (Show)
instance Exception CreateCardholderErrors

createCardholder
  :: (HasAccounts m, HasAptoClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> m CreateCardholderAction
createCardholder trace userID = do
  let mid = traceToMID trace
  liftIO $ putStr "createCardholder " >> print (userID, trace)

  userMaybe <- getUser trace userID

  let hasCardholderId :: Maybe AptoCardholderId =
        userMaybe >>= usrAptoCardholderID

  (action, res) <- case (userMaybe, hasCardholderId) of
    (Just user, Nothing) -> do
      liftIO $ putStr "createCardholder creating " >> print (userID, trace)
      (CardholderCreated, ) <$> Apto.createCardholder mid user


    (Just user, Just cid) -> do
      liftIO $ putStr "createCardholder updating " >> print (userID, cid, trace)
      (CardholderUpdated, ) <$> Apto.createCardholder mid user

    (Nothing, _) -> do
      liftIO $ putStr "Error: createCardholder missing user " >> print
        (userID, trace)
      throw MissingUser

  cardholderID <- case res of
    Left _ -> throw HtttpFailure
    Right AptoCardholderResponse { accxId = cardholderID } ->
      return cardholderID

  return . action $ AptoPaymentsCH cardholderID
