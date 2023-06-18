{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Foldable (toList)
import Data.Kind (Type)
import GHC.TypeNats (Nat, type (<=))
import Index (Index, SizeNat)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Fun,
    Gen,
    Property,
    applyFun,
    forAllShrinkShow,
    liftArbitrary,
    liftShrink,
    (===),
  )
import Test.QuickCheck.Poly (A)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Vector (Vector, reindex)

main :: IO ()
main =
  defaultMain . testGroup "Properties" $
    [ testGroup
        "Index"
        [ testGroup
            "Num laws"
            [ testProperty "x - x = minBound" $ numNegateLaw @(Index 1000),
              fromIntegralOpProperty @1000 (+) "+",
              fromIntegralOpProperty @1000 (*) "*",
              fromIntegralOpProperty @1000 (-) "-"
              {-
                testProperty "fromIntegral x + fromIntegral y = fromIntegral (x + y)" $ numFIPlusLaw @1000,
                testProperty "fromIntegral x * fromIntegral y = fromIntegral (x * y)" $ numFITimesLaw @1000
              -}
            ]
        ],
      testGroup
        "Vector"
        [ testProperty "reindex f . reindex g = reindex (g . f)" $ reindexLaw @100 @200 @300
        ]
    ]

-- Properties

reindexLaw ::
  forall (n :: Nat) (m1 :: Nat) (m2 :: Nat).
  (SizeNat n, 1 <= n, SizeNat m1, 1 <= m1, SizeNat m2, 1 <= m2) =>
  Property
reindexLaw = forAllShrinkShow gen reduce showCase $
  \(f, g, v) ->
    let f' = applyFun f
        g' = applyFun g
     in (toList . reindex f' . reindex g' $ v)
          === (toList . reindex (g' . f') $ v)
  where
    gen :: Gen (Fun (Index n) (Index m1), Fun (Index m1) (Index m2), Vector m2 A)
    gen = (,,) <$> arbitrary <*> arbitrary <*> liftArbitrary arbitrary
    reduce ::
      (Fun (Index n) (Index m1), Fun (Index m1) (Index m2), Vector m2 A) ->
      [(Fun (Index n) (Index m1), Fun (Index m1) (Index m2), Vector m2 A)]
    reduce (f, g, v) = (,,) <$> shrink f <*> shrink g <*> liftShrink shrink v
    showCase ::
      (Fun (Index n) (Index m1), Fun (Index m1) (Index m2), Vector m2 A) ->
      String
    showCase (f, g, v) = "(" <> show f <> ", " <> show g <> ", " <> (show . toList $ v) <> ")"

numNegateLaw ::
  forall (a :: Type).
  (Arbitrary a, Show a, Num a, Eq a, Bounded a) =>
  Property
numNegateLaw = forAllShrinkShow arbitrary shrink show $ \(x :: a) ->
  x - x === minBound

fromIntegralOpProperty ::
  forall (n :: Nat).
  (SizeNat n, 1 <= n) =>
  (forall (a :: Type). Num a => a -> a -> a) ->
  String ->
  TestTree
fromIntegralOpProperty op name =
  testProperty propName . forAllShrinkShow arbitrary shrink showArgs $ \(x, y) ->
    fromIntegral x `op` fromIntegral y === (fromIntegral @_ @(Index n) $ x `op` y)
  where
    propName :: String
    propName = "fromIntegral x " <> name <> " fromIntegral y = fromIntegral (x " <> name <> " y)"
    showArgs :: (Integer, Integer) -> String
    showArgs (x, y) = "fromIntegral " <> show x <> " " <> name <> " fromIntegral " <> show y

{- Draft 2:

numFIPlusLaw ::
  forall (n :: Nat).
  (SizeNat n, 1 <= n) =>
  Property
numFIPlusLaw = forAllShrinkShow arbitrary shrink showArgs $ \(x, y) ->
  fromIntegral x + fromIntegral y === (fromIntegral @_ @(Index n) $ x + y)
  where
    showArgs :: (Integer, Integer) -> String
    showArgs (x, y) = "fromIntegral " <> show x <> " + fromIntegral " <> show y

{- Draft 1:
numFIPlusLaw = forAllShrinkShow arbitrary shrink show $ \(x :: Integer, y :: Integer) ->
  fromIntegral x + fromIntegral y === (fromIntegral @_ @(Index n) $ x + y)
-}

numFITimesLaw ::
  forall (n :: Nat).
  (SizeNat n, 1 <= n) =>
  Property
numFITimesLaw = forAllShrinkShow arbitrary shrink showArgs $ \(x, y) ->
  fromIntegral x * fromIntegral y === (fromIntegral @_ @(Index n) $ x * y)
  where
    showArgs :: (Integer, Integer) -> String
    showArgs (x, y) = "fromIntegral " <> show x <> " * fromIntegral " <> show y

-}
