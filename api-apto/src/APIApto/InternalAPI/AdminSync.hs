module APIApto.InternalAPI.AdminSync where

import qualified APIApto.Cardholder.Sync       as CS
import           APIApto.Monad.Accounts         ( HasAccounts(..) )
import           APIApto.Monad.Apto             ( HasAptoClient )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Servant                        ( NoContent(..) )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.AccountsFSM.API  ( ChangeKYCStateBody(..) )
import           Shared.WebAPI.General.API      ( TraceContext(..)
                                                , incrementTrace
                                                )

pullSync
  :: (HasAccounts m, HasAptoClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> m NoContent
pullSync trace userId = do
  status <- CS.syncUserFromApto trace userId
  case status of
    Nothing        -> return ()
    Just newStatus -> do
      newTrace <- incrementTrace trace
      let statusBody = ChangeKYCStateBody newStatus
      setKycState newTrace userId statusBody

  return NoContent
