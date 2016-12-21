-- | In lens-4.14, the constraint @'Functor' f@ is missing from the definition
-- of 'to'. When compiling with GHC 8.0, this leads to warnings for definitions
-- like
--
-- @
-- foo :: Getter Bar Foo
-- foo = to fooFromBar
-- @
--
-- because of the redundant @'Functor' f@ constraint. This module exports an
-- alternative definition with the added @'Functor' f@ Constraint.
module Control.Lens.Compat
  ( to
  , module Control.Lens
  ) where

import           Control.Lens hiding (to)
import qualified Control.Lens as Lens


-- | Build an (index-preserving) 'Getter' from an arbitrary Haskell function.
--
-- Identical to "Control.Lens".'Lens.to' except for the additional constraint
-- @'Functor' f@.
to :: (Profunctor p, Functor f, Contravariant f) => (s -> a) -> Optic' p f s a
to k = fakeFunctorConstraintToDisableGhc8WarningWithLens4_14 . Lens.to k
  where
    fakeFunctorConstraintToDisableGhc8WarningWithLens4_14 getter =
        let _ = rmap (fmap undefined) getter
        in  getter
