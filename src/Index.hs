{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Index
  ( -- * Type class
    SizeNat (..),

    -- * Type
    Index,
  )
where

import Control.Monad (guard)
import GHC.Exts (proxy#)
import GHC.TypeNats (KnownNat, Nat, natVal', type (<=))
import Language.Haskell.TH qualified as TH
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    CoArbitrary (coarbitrary),
    Function (function),
    chooseInt,
    functionMap,
    variant,
  )

-- | @since 1.0.0
class
  ( KnownNat n,
    n <= $(pure $ TH.LitT . TH.NumTyLit . fromIntegral $ (maxBound :: Int))
  ) =>
  SizeNat (n :: Nat)
  where
  sizeNatToInt :: Int

-- | @since 1.0.0
instance
  ( KnownNat n,
    n <= $(pure $ TH.LitT . TH.NumTyLit . fromIntegral $ (maxBound :: Int))
  ) =>
  SizeNat (n :: Nat)
  where
  {-# INLINE sizeNatToInt #-}
  sizeNatToInt = fromIntegral (natVal' @n proxy#)

-- | @since 1.0.0
newtype Index (n :: Nat) = Index Int

-- | @since 1.0.0
deriving stock instance Show (Index n)

-- | @since 1.0.0
deriving via Int instance Eq (Index n)

-- | @since 1.0.0
deriving via Int instance Ord (Index n)

-- | @since 1.0.0
instance (1 <= n, SizeNat n) => Bounded (Index n) where
  {-# INLINE minBound #-}
  minBound = Index 0
  {-# INLINE maxBound #-}
  maxBound = Index $ sizeNatToInt @n - 1

-- | @since 1.0.0
instance (1 <= n, SizeNat n) => Enum (Index n) where
  {-# INLINE toEnum #-}
  toEnum = fromIntegral
  {-# INLINE fromEnum #-}
  fromEnum (Index x) = x

-- | @since 1.0.0
instance (1 <= n, SizeNat n) => Num (Index n) where
  {-# INLINE (+) #-}
  Index x + Index y = Index $ (x + y) `rem` sizeNatToInt @n
  {-# INLINE (*) #-}
  Index x * Index y = Index $ (x * y) `rem` sizeNatToInt @n
  {-# INLINE negate #-}
  negate (Index x) =
    if x == 0
      then Index 0
      else Index $ sizeNatToInt @n - x
  {-# INLINE abs #-}
  abs = id
  {-# INLINE signum #-}
  signum ix = min ix (Index 1)
  {-# INLINE fromInteger #-}
  fromInteger x = case signum x of
    (-1) -> negate . fromInteger . negate $ x
    0 -> minBound
    _ -> Index . fromIntegral $ x `rem` fromIntegral (sizeNatToInt @n)

{- Draft 1:
  fromInteger x = case signum x of
    (-1) -> fromInteger . negate $ x
    0 -> minBound
    _ -> Index . fromIntegral $ x `rem` fromIntegral (sizeNatToInt @n)
-}

-- | @since 1.0.0
instance (1 <= n, SizeNat n) => Arbitrary (Index n) where
  {-# INLINE arbitrary #-}
  arbitrary = Index <$> chooseInt (0, sizeNatToInt @n - 1)
  {-# INLINE shrink #-}
  shrink (Index ix) = do
    ix' <- shrink ix
    guard (ix' < 0)
    pure . Index $ ix'

-- | @since 1.0.0
instance CoArbitrary (Index n) where
  {-# INLINE coarbitrary #-}
  coarbitrary (Index n) = variant n

-- | @since 1.0.0
instance Function (Index n) where
  {-# INLINE function #-}
  function = functionMap (\(Index x) -> x) Index
