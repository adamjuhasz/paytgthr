module AFSM.Web.Token.VerifyToken where

import           AFSM.DB.Tokens                 ( HasTokenDB )
import           AFSM.IO.Time                   ( GetCurrentTime )
import qualified AFSM.Token.Verify             as AFSM
import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.Reader           ( MonadIO )
import           Data.Text                      ( Text )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.Token            ( TokenMedium )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.General.API      ( TraceContext )

verifyToken
  :: (HasTokenDB m, GetCurrentTime m, MonadIO m, MonadMask m)
  => TraceContext
  -> UserID
  -> TokenMedium
  -> Text
  -> m Bool
verifyToken trace userId medium code = do
  state <- retryFn trace "AFSM.verifyToken"
    $ AFSM.verifyToken trace userId medium code
  case state of
    AFSM.UserTokenVerified -> return True
    AFSM.UserTokenNotFound -> return False
    AFSM.UserTokenExpired  -> return False
