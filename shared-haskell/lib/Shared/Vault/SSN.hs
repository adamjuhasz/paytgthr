{- HLINT ignore "Use lambda-case" -}

module Shared.Vault.SSN where

import           Data.Aeson.Types               ( Object
                                                , Value(String)
                                                )
import qualified Data.HashMap.Strict           as HMS
import           GHC.Stack                      ( HasCallStack )
import           Shared.Vault                   ( extractSecret
                                                , Context(..)
                                                )

getSSNSecrets :: HasCallStack => (String -> IO Object) -> IO (String, Context)
getSSNSecrets secretGetter = do
  secrets <- secretGetter "ssn_transit/enc_secrets"

  let ssnKey     = extractSecret secrets "ssn_key"
      ssnContext = case secrets HMS.! "ssn_context" of
        String t -> Context t
        _        -> error "bad value for ssn_context"
  return (ssnKey, ssnContext)
