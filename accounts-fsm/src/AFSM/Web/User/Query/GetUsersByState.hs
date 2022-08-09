module AFSM.Web.User.Query.GetUsersByState where

import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB )
import qualified AFSM.User.Query.User          as AFSM
import           Shared.Models.User             ( UserID )
import           Shared.WebAPI.AccountsFSM.API  ( TraceContext )

getActiveUsers :: (HasGetUserDB m) => TraceContext -> m [UserID]
getActiveUsers _ = AFSM.getActiveUsers
