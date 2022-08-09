module APIDwolla.InternalAPI.AccountCreate where

import qualified APIDwolla.Account.Create      as AC
import           APIDwolla.Account.Create       ( CreateOptions(AddBankIfNeeded)
                                                )
import           APIDwolla.InternalAPI.ProcessChanges
                                                ( processChanges )
import           APIDwolla.Monad.Accounts       ( HasAccounts )
import           APIDwolla.Monad.Dwolla         ( HasDwollaClient )
import           APIDwolla.Monad.Payment        ( HasPayments(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.ApiDwolla.API    ( CreateDwollaAccountResponse(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

createAccount
  :: (HasDwollaClient m, HasAccounts m, HasPayments m, MonadIO m)
  => TraceContext
  -> UserID
  -> m CreateDwollaAccountResponse
createAccount trace userId = do
  tracePrint trace "APIDwolla.InternalAPI.AccountCreate.createAccount " userId

  -- updates to user happen inside `createDwollaAccount`
  changes <- AC.createDwollaAccount trace userId AddBankIfNeeded
  mapM_ (processChanges trace userId) changes

  return CreateDwollaAccountResponse
