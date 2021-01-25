{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}
module Control.Lens.Compat
  ( pre
  , assign
  , modifying
  , traverseOf

  , Getter

  , module Lens.Micro.Platform
  ) where

import Data.Monoid         (First)
import Lens.Micro.Platform hiding (assign, modifying, traverseOf)
import Control.Monad.State (MonadState, modify)


pre :: Getting (First a) s a -> Getter s (Maybe a)
pre l = to (preview l)
{-# INLINE pre #-}

assign :: MonadState s m => ASetter s s a b -> b -> m ()
assign l b = modify (set l b)
{-# INLINE assign #-}

modifying :: MonadState s m => ASetter s s a b -> (a -> b) -> m ()
modifying l f = modify (over l f)
{-# INLINE modifying #-}

traverseOf :: a -> a
traverseOf = id
{-# INLINE traverseOf #-}

type Getter s a = SimpleGetter s a

-- | Build an (index-preserving) 'Getter' from an arbitrary Haskell function.
-- See "Control.Lens".'Lens.to' for details.
--
-- In <https://hackage.haskell.org/package/lens-4.14 lens-4.14>, the constraint
-- @'Functor' f@ is missing from the definition of 'Lens.to'. When compiling
-- with GHC 8.0, this leads to warnings for definitions like
--
-- @
-- foo :: Getter Bar Foo
-- foo = to fooFromBar
-- @
--
-- because of the redundant @'Functor' f@ constraint. This definition is
-- identical to "Control.Lens".'Lens.to' except for the additional constraint
-- @'Functor' f@.
-- to :: (Profunctor p, Functor f, Contravariant f) => (s -> a) -> Optic' p f s a
-- to k = getter
--   where
--     getter = Lens.to k
--     _fakeFunctorConstraint = rmap (fmap undefined) . getter
