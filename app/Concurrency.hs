{-# LANGUAGE ScopedTypeVariables #-}

module Concurrency (jobLimit, runBounded) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (SomeException, bracket_, try)

-- | CPU-relative job cap: max(4, capabilities * 2).
jobLimit :: IO Int
jobLimit = do
  capabilities <- getNumCapabilities
  return $ max 4 (capabilities * 2)

-- | Run jobs concurrently with a semaphore-bounded worker count.
-- The status callback is invoked before and after each job.
runBounded
  :: forall k a. Int
  -> [(k, IO a)]
  -> (k -> IO ())                       -- onStart
  -> (k -> Either String a -> IO ())    -- onFinish
  -> IO [(k, Either String a)]
runBounded limit jobs onStart onFinish = do
  semaphore <- newQSem limit
  mapConcurrently (runOne semaphore) jobs
  where
    runOne :: QSem -> (k, IO a) -> IO (k, Either String a)
    runOne semaphore (key, action) =
      bracket_ (waitQSem semaphore) (signalQSem semaphore) $ do
        onStart key
        outcome <- try action :: IO (Either SomeException a)
        let result = either (Left . show) Right outcome
        onFinish key result
        return (key, result)
