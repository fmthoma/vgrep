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

newtype TPQueue k v = TPQueue (TVar (PQueue k v))

mkTPQueue :: Functor f => f (TVar (PQueue k v)) -> f (TPQueue k v)
mkTPQueue = fmap TPQueue


newTPQueue :: Ord k => STM (TPQueue k v)
newTPQueue = mkTPQueue (newTVar PQueue.empty)

newTPQueueIO :: Ord k => IO (TPQueue k v)
newTPQueueIO = mkTPQueue (newTVarIO PQueue.empty)

writeTPQueue :: Ord k => TPQueue k v -> k -> v -> STM ()
writeTPQueue (TPQueue h) k v = modifyTVar' h (PQueue.add k v)

readTPQueue :: Ord k => TPQueue k v -> STM v
readTPQueue (TPQueue h) = do
    xs <- readTVar h
    case PQueue.minView xs of
        Just (x, xs') -> writeTVar h xs' >> pure x
        Nothing       -> retry

tryReadTPQueue :: Ord k => TPQueue k v -> STM (Maybe v)
tryReadTPQueue (TPQueue h) = do
    xs <- readTVar h
    case PQueue.minView xs of
        Just (x, xs') -> writeTVar h xs' >> pure (Just x)
        Nothing       -> pure Nothing

peekTPQueue :: Ord k => TPQueue k v -> STM v
peekTPQueue (TPQueue h) = do
    xs <- readTVar h
    case PQueue.minView xs of
        Just (x, _) -> pure x
        Nothing     -> retry

tryPeekTPQueue :: Ord k => TPQueue k v -> STM (Maybe v)
tryPeekTPQueue (TPQueue h) = do
    xs <- readTVar h
    case PQueue.minView xs of
        Just (x, _) -> pure (Just x)
        Nothing     -> pure Nothing

isEmptyTPQueue :: Ord k => TPQueue k v -> STM Bool
isEmptyTPQueue (TPQueue h) = fmap PQueue.null (readTVar h)
