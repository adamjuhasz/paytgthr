module AFSM.Monad.HasSaveUserDB where

import           Data.Text                      ( Text )
import           Shared.Models.Card             ( CardModel )
import           Shared.Models.KYCAssesment     ( KYCAssesment )
import           Shared.Models.User             ( UserID
                                                , UserModel
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

type IPAddress = Text
type IPCountry = Text
type Revision = Int

class Monad m => HasSaveUserDB m where
  saveUserModel :: UserModel -> m ()
  saveCardModel :: TraceContext -> CardModel -> m ()
  saveAssesment :: TraceContext -> KYCAssesment -> m ()
  saveUserNote  :: TraceContext -> UserID -> (Text, Revision) -> m ()
  saveDeviceIP  :: TraceContext -> UserID -> (IPAddress, IPCountry) -> m ()
