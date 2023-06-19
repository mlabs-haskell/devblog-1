{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Vector (Vector, reindex) where

import Data.Kind (Type)
import GHC.TypeNats (Nat, type (<=))
import Index (Index, SizeNat)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Arbitrary1 (liftArbitrary, liftShrink))

-- Problem with persistent data structures is that they are all pointer-based.
-- This is bad for performance!
--
-- Arrays (in the assembly sense) are good:
--

-- * Contiguous in memory (good spatial locality)

-- * Cache friendly (good temporal locality)

{-
--
-- In the functional (or persistent) sense, arrays are pretty bad: either we
-- have to constantly copy them, or we have to work in a mutable context. Both
-- of these are annoying, because copying is wasteful and expensive, and mutable
-- contexts lack convience APIs (even Functor is impossible).
--
-- fmap f v will copy v, but then throw v away!
--
-- We would like something in-between: hence, pull arrays.

newtype PullArray (a :: Type) = PullArray (Int -> a) -- function from indexes to values

instance Functor PullArray where
  {-# INLINE fmap #-}
  fmap f (PullArray g) = PullArray (f . g)

reindex :: forall (a :: Type) . (Int -> Int) -> PullArray a -> PullArray a
reindex f (PullArray g) = PullArray $ contramap f g
-}

-- | @since 1.0.0
newtype Vector (n :: Nat) (a :: Type)
  = Vector (Index n -> a)

-- | @since 1.0.0
deriving via ((->) (Index n)) instance Functor (Vector n)

-- | @since 1.0.0
instance (SizeNat n, 1 <= n) => Foldable (Vector n) where
  {-# INLINE foldMap #-}
  foldMap f (Vector g) = foldMap (f . g) [minBound .. maxBound]

-- | @since 1.0.0
instance Arbitrary a => Arbitrary (Vector n a) where
  {-# INLINE arbitrary #-}
  arbitrary = Vector <$> arbitrary
  {-# INLINE shrink #-}
  shrink (Vector f) = Vector <$> shrink f

-- | @since 1.0.0
reindex ::
  forall (n :: Nat) (m :: Nat) (a :: Type).
  (Index n -> Index m) ->
  Vector m a ->
  Vector n a
reindex f (Vector g) = Vector $ g . f
