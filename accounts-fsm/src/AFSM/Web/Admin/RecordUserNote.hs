module AFSM.Web.Admin.RecordUserNote where

import           AFSM.Monad.HasEventTracking    ( HasEventTracking(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(saveUserNote) )
import           Data.Text                      ( Text )
import           Servant.API                    ( NoContent(..) )
import           Shared.Models.Base             ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext )

recordUserNote
  :: (HasGetUserDB m, HasSaveUserDB m, HasEventTracking m)
  => TraceContext
  -> UserID
  -> Text
  -> m NoContent
recordUserNote trace uid note = do
  lastNote <- getUserNote trace uid

  let rev = case lastNote of
        Nothing     -> 1
        Just (_, r) -> r + 1

  saveUserNote trace uid (note, rev)

  trackEvent uid "User Note Made"

  return NoContent
