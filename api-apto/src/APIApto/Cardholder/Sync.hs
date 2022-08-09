module APIApto.Cardholder.Sync where

import           APIApto.Monad.Accounts         ( HasAccounts(getUser) )
import           APIApto.Monad.Apto            as Apto
                                                ( HasAptoClient(getCardholder) )
import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Shared.Models.Apto.Cardholder  ( AptoCardholderResponse(..) )
import           Shared.Models.KYC              ( KycStatus )
import           Shared.Models.User             ( UserID
                                                , UserModel(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

data SyncErrors
  = NoCardholderID
  | HttpFailure
  | MissingKYCStatus
  deriving (Show)
instance Exception SyncErrors

syncUserFromApto
  :: (HasAccounts m, HasAptoClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> m (Maybe KycStatus)
syncUserFromApto trace sacUser = do
  let mid = traceToMID trace
  user     <- getUser trace sacUser

  crdholdr <- case user >>= usrAptoCardholderID of
    Nothing       -> throw NoCardholderID
    Just crdholdr -> return crdholdr


  res <- getCardholder mid crdholdr

  case res of
    Left e -> do
      liftIO $ putStr "Error: SyncUserWithApto can't get cardholder " >> print
        (trace, sacUser, e)
      throw HttpFailure
    Right response@AptoCardholderResponse { accxKYCStatus = Nothing } -> do
      liftIO
        $  putStr "Error: SyncUserWithApto accxKYCStatus is Nothing: "
        >> print (trace, sacUser, response, user)
      throw MissingKYCStatus
    Right response@AptoCardholderResponse { accxKYCStatus = Just aptoKYCStatus }
      -> if Just aptoKYCStatus /= (user >>= usrAptoKYCStatus)
        then do
          liftIO $ putStr "Error: SyncUserWithApto will cause change: " >> print
            (trace, sacUser, response, user)
          return $ Just aptoKYCStatus
        else return Nothing
