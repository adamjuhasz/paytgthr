module APIPrivacy.Monad.HasEnvironment where

data RunningEnvironment
  = Production
  | Staging
  | Development
  deriving (Eq, Show, Read)
