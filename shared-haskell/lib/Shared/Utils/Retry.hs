{-# LANGUAGE RecordWildCards #-}

module Shared.Utils.Retry where

import           Control.Monad                  ( when )
import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Retry                  ( RetryPolicyM
                                                , RetryStatus(..)
                                                , exponentialBackoff
                                                , fullJitterBackoff
                                                , limitRetries
                                                , recoverAll
                                                , retrying
                                                )
import           Shared.Console                 ( tracePrint )
import           Shared.WebAPI.General.API      ( TraceContext )

retryEither
  :: (MonadIO m)
  => TraceContext
  -> String
  -> Int
  -> m (Either a b)
  -> m (Either a b)
retryEither trace debugStr retryMs fn = do
  let policy = fullJitterBackoff retryMs <> limitRetries 5
  let isLeft _ (Left  _) = return True
      isLeft _ (Right _) = return False
  let runner RetryStatus {..} = do
        when
          (rsIterNumber > 0)
          (tracePrint
            trace
            (debugStr <> " ")
            ( ("Retry no:" :: String        , rsIterNumber)
            , ("Cumulative Delay:" :: String, rsCumulativeDelay)
            )
          )
        fn
  retrying policy isLeft runner

retryFn :: (MonadMask m, MonadIO m) => TraceContext -> String -> m a -> m a
retryFn = retryFnWith (exponentialBackoff 50 <> limitRetries 5)

retryFnJitter
  :: (MonadMask m, MonadIO m) => TraceContext -> String -> m a -> m a
retryFnJitter = retryFnWith (fullJitterBackoff 50 <> limitRetries 10)

retryFnWith
  :: (MonadMask m, MonadIO m)
  => RetryPolicyM m
  -> TraceContext
  -> String
  -> m a
  -> m a
retryFnWith pol trace debugStr fn = recoverAll pol $ \RetryStatus {..} -> do
  when
    (rsIterNumber > 0)
    (tracePrint
      trace
      (debugStr <> " ")
      ( ("Retry no:" :: String        , rsIterNumber)
      , ("Cumulative Delay:" :: String, rsCumulativeDelay)
      )
    )
  -- now run the function
  fn
