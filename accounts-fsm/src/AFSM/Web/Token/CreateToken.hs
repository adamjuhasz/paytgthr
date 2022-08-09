module AFSM.Web.Token.CreateToken where

import           AFSM.DB.Tokens                 ( HasTokenDB )
import           AFSM.IO.Time                   ( GetCurrentTime )
import           AFSM.Monad.HasEventTracking    ( HasEventTracking )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB )
import qualified AFSM.Token.Create             as AFSM
import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.Reader           ( MonadIO )
import           Servant.API                    ( NoContent(..) )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.Token            ( ExpirationTime
                                                , TokenId
                                                , TokenMedium
                                                )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.General.API      ( TraceContext )

createToken
  :: ( HasTokenDB m
     , HasGetUserDB m
     , GetCurrentTime m
     , HasEventTracking m
     , MonadIO m
     , MonadMask m
     )
  => TraceContext
  -> UserID
  -> TokenMedium
  -> ExpirationTime
  -> TokenId
  -> m NoContent
createToken trace userId medium expire tokenId = do
  _token <- retryFn trace "AFSM.createToken"
    $ AFSM.createToken trace userId medium tokenId expire
  return NoContent
