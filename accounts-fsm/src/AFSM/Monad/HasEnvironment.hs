module AFSM.Monad.HasEnvironment where

data RunningEnvironment
  = Production
  | Staging
  | Development
  deriving (Eq, Show, Read)

class Monad m => HasEnvironment m where
  getEnvironment :: m RunningEnvironment
