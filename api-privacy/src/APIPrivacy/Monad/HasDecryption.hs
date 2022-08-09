module APIPrivacy.Monad.HasDecryption where

import           Shared.Vault                   ( CipherText
                                                , PlainText
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

class Monad m => HasDecryption m where
  decryptSSN :: TraceContext -> CipherText -> m PlainText
  decryptPIN :: TraceContext -> CipherText -> m PlainText
