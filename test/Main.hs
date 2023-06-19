{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Foldable (toList)
import GHC.TypeNats (Nat, type (<=))
import Index (Index, SizeNat)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Fun,
    Property,
    applyFun,
    counterexample,
    forAllShrinkShow,
    (===),
  )
import Test.QuickCheck.Poly (A)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Vector (Vector (Vector), reindex)

main :: IO ()
main =
  defaultMain . adjustOption moreTests . testGroup "Properties" $
    [ testGroup
        "Index"
        [ testProperty "fromInteger i + fromInteger j = fromInteger (i + j)" $ fiProp1 @1000
        ],
      testGroup
        "Vector"
        [ testProperty "reindex f . reindex g = reindex (g . f)" $ reindexProp @100 @200 @300
        ]
    ]
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 10_000

-- Properties

reindexProp ::
  forall (n1 :: Nat) (n2 :: Nat) (n3 :: Nat).
  (SizeNat n1, 1 <= n1, SizeNat n2, 1 <= n2, SizeNat n3, 1 <= n3) =>
  Property
reindexProp = forAllShrinkShow arbitrary shrink show $
  \(testData :: ReindexPropData n1 n2 n3) ->
    let (f, g, v) = toReindexPropData testData
     in toList (reindex f . reindex g $ v) === toList (reindex (g . f) v)

fiProp1 ::
  forall (n :: Nat).
  (SizeNat n, 1 <= n) =>
  Property
fiProp1 = forAllShrinkShow arbitrary shrink showCases $ \(i, j) ->
  let ixI :: Index n = fromInteger i
      ixJ :: Index n = fromInteger j
   in counterexample ("fromInteger i = " <> show ixI)
        . counterexample ("fromInteger j = " <> show ixJ)
        $ ixI + ixJ === fromInteger (i + j)
  where
    showCases :: (Integer, Integer) -> String
    showCases (i, j) = "i = " <> show i <> ", j = " <> show j

-- Helpers

data ReindexPropData (n1 :: Nat) (n2 :: Nat) (n3 :: Nat)
  = ReindexPropData
      (Fun (Index n1) (Index n2))
      (Fun (Index n2) (Index n3))
      (Fun (Index n3) A)

deriving stock instance Show (ReindexPropData n1 n2 n3)

instance
  (SizeNat n1, 1 <= n1, SizeNat n2, 1 <= n2, SizeNat n3, 1 <= n3) =>
  Arbitrary (ReindexPropData n1 n2 n3)
  where
  arbitrary = ReindexPropData <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (ReindexPropData f g v) =
    ReindexPropData <$> shrink f <*> shrink g <*> shrink v

toReindexPropData ::
  forall (n1 :: Nat) (n2 :: Nat) (n3 :: Nat).
  ReindexPropData n1 n2 n3 ->
  (Index n1 -> Index n2, Index n2 -> Index n3, Vector n3 A)
toReindexPropData (ReindexPropData f g v) =
  (applyFun f, applyFun g, Vector . applyFun $ v)
