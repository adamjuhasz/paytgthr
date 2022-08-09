module AFSM.Monad.HasDecryption
  ( module AFSM.Monad.HasDecryption
  , CipherText(..)
  , PlainText(..)
  ) where

import           Shared.Models.User             ( UserID )
import           Shared.Vault                   ( CipherText(..)
                                                , PlainText(..)
                                                )

class Monad m => HasDecryption m where
  decryptSSN :: UserID -> CipherText -> m PlainText
