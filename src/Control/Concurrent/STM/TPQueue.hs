-- | A transactional priority queue, based on a Finger Tree.
module Control.Concurrent.STM.TPQueue
  ( TPQueue ()
  , newTPQueue
  , newTPQueueIO
  , writeTPQueue
  , readTPQueue
  , tryReadTPQueue
  , peekTPQueue
  , tryPeekTPQueue
  , isEmptyTPQueue
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQueue

-- | 'TPQueue' is an unbounded priority queue based on a Finger Tree.
newtype TPQueue k v = TPQueue (TVar (PQueue k v))

mkTPQueue :: Functor f => f (TVar (PQueue k v)) -> f (TPQueue k v)
mkTPQueue = fmap TPQueue

-- | Build a new 'TPQueue'.
newTPQueue :: Ord k => STM (TPQueue k v)
newTPQueue = mkTPQueue (newTVar PQueue.empty)

-- | IO version of 'newTPQueue'. This is useful for creating top-level
-- 'TPQueues' using 'unsafePerformIO', because using 'atomically' inside
-- 'unsafePerformIO' isn't possible.
newTPQueueIO :: Ord k => IO (TPQueue k v)
newTPQueueIO = mkTPQueue (newTVarIO PQueue.empty)

-- | Write a value to a 'TPQueue'.
writeTPQueue :: Ord k => TPQueue k v -> k -> v -> STM ()
writeTPQueue (TPQueue h) k v = modifyTVar' h (PQueue.add k v)

-- | Read the next minimal value from a 'TPQueue'.
readTPQueue :: Ord k => TPQueue k v -> STM v
readTPQueue (TPQueue h) = do
    xs <- readTVar h
    case PQueue.minView xs of
        Just (x, xs') -> writeTVar h xs' >> pure x
        Nothing       -> retry

-- | A version of 'readTPQueue' that does not retry, but returns 'Nothing'
-- instead if no value is available.
tryReadTPQueue :: Ord k => TPQueue k v -> STM (Maybe v)
tryReadTPQueue (TPQueue h) = do
    xs <- readTVar h
    case PQueue.minView xs of
        Just (x, xs') -> writeTVar h xs' >> pure (Just x)
        Nothing       -> pure Nothing

-- | Get the next minimal value from a 'TPQueue' without removing it.
peekTPQueue :: Ord k => TPQueue k v -> STM v
peekTPQueue (TPQueue h) = do
    xs <- readTVar h
    case PQueue.minView xs of
        Just (x, _) -> pure x
        Nothing     -> retry

-- | A version of 'peekTPQueue' that does not retry, but returns 'Nothing'
-- instead if no value is available.
tryPeekTPQueue :: Ord k => TPQueue k v -> STM (Maybe v)
tryPeekTPQueue (TPQueue h) = do
    xs <- readTVar h
    case PQueue.minView xs of
        Just (x, _) -> pure (Just x)
        Nothing     -> pure Nothing

-- | Returns 'True' if the 'TPQueue' is empty.
isEmptyTPQueue :: Ord k => TPQueue k v -> STM Bool
isEmptyTPQueue (TPQueue h) = fmap PQueue.null (readTVar h)
