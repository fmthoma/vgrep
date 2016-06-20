module Pipes.Concurrent.PQueue
  ( spawn
  , withSpawn
  , module Pipes.Concurrent
  ) where

import           Control.Applicative
import           Control.Concurrent.STM         as STM
import qualified Control.Concurrent.STM.TPQueue as TPQueue
import           Control.Exception              (bracket)
import           Control.Monad
import           Pipes.Concurrent
    ( Input (..)
    , Output (..)
    , fromInput
    , toOutput
    )


spawn :: Ord p => IO (Output (p, a), Input a, STM ())
spawn = do
    q <- TPQueue.newTPQueueIO
    sealed <- STM.newTVarIO False
    let seal = STM.writeTVar sealed True

    {- Use weak TVars to keep track of whether the 'Input' or 'Output' has been
       garbage collected.  Seal the mailbox when either of them becomes garbage
       collected.
    -}
    rSend <- STM.newTVarIO ()
    void (STM.mkWeakTVar rSend (STM.atomically seal))
    rRecv <- STM.newTVarIO ()
    void (STM.mkWeakTVar rRecv (STM.atomically seal))

    let sendOrEnd p a = do
            isSealed <- readTVar sealed
            if isSealed
                then pure False
                else TPQueue.writeTPQueue q p a >> pure True

        readOrEnd = fmap Just (TPQueue.readTPQueue q)
                <|> (readTVar sealed >>= check >> pure Nothing)

        _send (p, a) = sendOrEnd p a <* readTVar rSend
        _recv        = readOrEnd     <* readTVar rRecv
    return (Output _send, Input _recv, seal)
{-# INLINABLE spawn #-}

withSpawn :: Ord p => ((Output (p, a), Input a) -> IO r) -> IO r
withSpawn action = bracket spawn
    (\(_, _, seal) -> atomically seal)
    (\(output, input, _) -> action (output, input))
