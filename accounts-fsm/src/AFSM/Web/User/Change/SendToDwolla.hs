module AFSM.Web.User.Change.SendToDwolla where

import           Servant                        ( NoContent(..) )
import           Shared.Models.User             ( UserID )
import           Shared.WebAPI.ApiDwolla.Client ( CreateDwollaAccountResponse(..)
                                                , HasDwollaClient(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

sendToDwolla :: (HasDwollaClient m) => TraceContext -> UserID -> m NoContent
sendToDwolla trace userId = do
  CreateDwollaAccountResponse <- createDwollaAccount trace userId
  return NoContent
