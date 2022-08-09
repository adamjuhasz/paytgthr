module AFSM.Monad.HasEventTracking
  ( module AFSM.Monad.HasEventTracking
  , object
  , (.=)
  ) where

import           Data.Aeson                     ( (.=)
                                                , Value
                                                , object
                                                )
import           Data.Text                      ( Text )
import           Shared.Models.User             ( UserID )

class Monad m => HasEventTracking m where
  trackEventWithProps :: UserID -> Text -> Value -> m ()
  trackEvent :: UserID -> Text -> m ()
  trackUser :: UserID -> m ()
  trackOneOffTrait :: UserID -> [(Text, Value)] -> m ()
  trackEventToCustomerIO :: UserID -> Text -> Value -> m ()
