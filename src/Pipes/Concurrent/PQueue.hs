-- | A variant of "Pipes.Concurrent" that uses a Finger Tree-based Priority
-- Queue ('TPQueue.TPQueue') instead of a normal 'TQueue'.
module Pipes.Concurrent.PQueue
  ( spawn
  , withSpawn
  -- * Re-exports from "Pipes.Concurrent"
  , Input (..)
  , Output (..)
  , fromInput
  , toOutput
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


-- | Spawn a mailbox to store prioritized messages in a Mailbox. Using 'recv' on
-- the 'Input' will return 'Just' the minimal element, or 'Nothing' if the
-- mailbox is closed.
--
-- This function is analogous to
-- @"Pipes.Concurrent".'Pipes.Concurrent.spawn'' 'Pipes.Concurrent.Unbounded'@,
-- but it uses a 'TPQueue.TPQueue' instead of a 'TQueue' to store messages.
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

-- | 'withSpawn' passes its enclosed action an 'Output' and 'Input' like you'd
-- get from 'spawn', but automatically @seal@s them after the action completes.
-- This can be used when you need the @seal@ing behavior available from 'spawn',
-- but want to work at a bit higher level:
--
-- > withSpawn buffer $ \(output, input) -> ...
--
-- 'withSpawn' is exception-safe, since it uses 'bracket' internally.
withSpawn :: Ord p => ((Output (p, a), Input a) -> IO r) -> IO r
withSpawn action = bracket spawn
    (\(_, _, seal) -> atomically seal)
    (\(output, input, _) -> action (output, input))
