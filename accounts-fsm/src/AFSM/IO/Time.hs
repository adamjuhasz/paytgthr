module AFSM.IO.Time
  ( GetCurrentTime(..)
  )
where

import           Data.Time.Clock                ( UTCTime )

class Monad m => GetCurrentTime m where
 getCurrentTime :: m UTCTime
