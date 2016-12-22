module Control.Lens.Compat
  ( to
  , module Control.Lens
  ) where

import           Control.Lens hiding (to)
import qualified Control.Lens as Lens


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
to :: (Profunctor p, Functor f, Contravariant f) => (s -> a) -> Optic' p f s a
to k = getter
  where
    getter = Lens.to k
    _fakeFunctorConstraint = rmap (fmap undefined) . getter
