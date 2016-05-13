{-# LANGUAGE Rank2Types #-}
module Vgrep.Event (
    -- * Event handling
    -- | An event handler is a function
    --
    -- @
    -- handleEvent :: 'Control.Monad.State.MonadState' s m => e -> s -> 'Next' (m 'Redraw')
    -- @
    --
    -- where @e@ is the event type and @s@ is the state of the handler. The
    -- 'Next' type determines the type of action to be performed. The state
    -- @s@ is passed as a parameter so the handler can decide which type of
    -- action to perform, while not being able to modify the state.
    --
    -- Event handlers form a 'Monoid' where the first handler that triggers
    -- will perform the action:
    --
    -- @
    -- (handleSome <> handleOther) event state
    -- @
    --
    -- is identical to
    --
    -- @
    -- case handleSome event state of
    --     Skip -> handleOther event state
    --     action -> action
    -- @
      Next (..)
    , Redraw (..)
    , Interrupt (..)

    -- * Dispatching Events
    , dispatch
    , dispatchMap

    -- ** Re-exports
    , module Data.Map
    ) where

import Control.Monad.IO.Class
import Data.Map (Map, fromList)
import qualified Data.Map as M

import Vgrep.Environment


-- | The type of action to be performed on an event.
data Next a
    = Skip
    -- ^ Do not handle the event (fall-through to other event handlers)

    | Continue a
    -- ^ Handle the event by performing an action

    | Interrupt Interrupt
    -- ^ Interrupt the application

-- | The first event handler that triggers (i. e. does not return 'Skip')
-- handles the event.
instance Monoid (Next a) where
    mempty = Skip
    Skip        `mappend` next       = next
    next        `mappend` _other     = next

instance Functor Next where
    fmap f = \case Skip        -> Skip
                   Continue a  -> Continue (f a)
                   Interrupt i -> Interrupt i

data Redraw
    = Redraw
    -- ^ Indicates that the state has been changed visibly, so the screen
    -- should be refreshed.

    | Unchanged
    -- ^ The state has not changed or the change would not be visible, so
    -- refreshing the screen is not required.

instance Monoid Redraw where
    mempty = Unchanged
    Unchanged `mappend` Unchanged = Unchanged
    _         `mappend` _         = Redraw


data Interrupt
    = Suspend (forall m. MonadIO m => Environment -> m ())
    -- ^ Suspend the application and run the action, e. g. invoking an
    -- external process, then resume the application.

    | Halt
    -- ^ Shut down.



-- | If the lookup returns @'Just' action@, then handle it with
-- @'Continue' action'@, otherwise 'Skip' this event handler.
dispatch :: (e -> Maybe a) -> e -> Next a
dispatch f = maybe Skip Continue . f

-- | Special case of 'dispatch' where actions are looked up from a map.
dispatchMap :: Ord e => Map e a -> e -> Next a
dispatchMap m = dispatch (`M.lookup` m)
