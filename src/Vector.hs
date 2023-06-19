{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Vector
  ( Vector (..),
    reindex,
  )
where

import Data.Kind (Type)
import GHC.TypeNats (Nat)
import Index (Index, SizeNat, indices)

-- | @since 1.0.0
newtype Vector (n :: Nat) (a :: Type)
  = Vector (Index n -> a)

-- | @since 1.0.0
deriving via ((->) (Index n)) instance Functor (Vector n)

-- | @since 1.0.0
instance SizeNat n => Foldable (Vector n) where
  {-# INLINE foldMap #-}
  foldMap f (Vector g) = foldMap (f . g) $ indices @n

-- | @since 1.0.0
reindex ::
  forall (n :: Nat) (m :: Nat) (a :: Type).
  (Index n -> Index m) ->
  Vector m a ->
  Vector n a
reindex f (Vector g) = Vector $ g . f
