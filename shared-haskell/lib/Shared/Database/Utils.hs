module Shared.Database.Utils where

import           Control.Concurrent
import           Control.Monad
import           Data.Pool
import           Data.Text                      ( Text )
import           Database.PostgreSQL.Simple
import           Shared.Utils
import           System.Metrics
import           System.Metrics.Distribution

timeDB :: Pool Connection -> Distribution -> (Connection -> IO a) -> IO a
timeDB pool dist fn = withResource pool $ \conn -> do
  (time, x) <- timeIO Nothing $ fn conn
  add dist (time * 1000)
  return x

timedSampler :: Store -> IO ()
timedSampler store =
  forever
    $  threadDelay (60000000 * 10) -- every 10 minutes
    >> putStr "samples: "
    >> (sampleAll store >>= print)

ekgRunner
  :: (Text -> Text -> IO Distribution)
  -> Pool Connection
  -> Text
  -> Text
  -> (Connection -> IO b)
  -> IO b
ekgRunner distSingleton psqlPool prefix name action = do
  aDist <- distSingleton prefix name
  timeDB psqlPool aDist action
