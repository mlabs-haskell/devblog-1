{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import GHC.TypeNats (Nat, type (<=))
import Index (Index, SizeNat)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Property,
    counterexample,
    forAllShrinkShow,
    (===),
  )
import Test.QuickCheck.Poly (A)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import Vector (Vector, reindex)

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

reindexProp ::
  forall (n1 :: Nat) (n2 :: Nat) (n3 :: Nat).
  (SizeNat n1, 1 <= n1, SizeNat n2, 1 <= n2, SizeNat n3, 1 <= n3) =>
  Property
reindexProp = forAllShrinkShow arbitrary shrink show $
  \(f :: Index n1 -> Index n2, g :: Index n2 -> Index n3, v :: Vector n3 A) ->
    (reindex f . reindex g $ v) === reindex (g . f) v

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

{-
main :: IO ()
main =
  defaultMain . adjustOption moreTests . testGroup "Properties" $
    [ testGroup
        "Index"
        [ testGroup
            "Num laws"
            [ testProperty "x - x = minBound" $ numNegateLaw @(Index 1000),
              fromIntegralOpProperty @1000 (+) "+",
              fromIntegralOpProperty @1000 (*) "*",
              testProperty "abs (a * b) = abs a * abs b" $ numAbsLaw1 @1000,
              testProperty "abs (a + b) <= abs a + abs b" $ numAbsLaw2 @1000
              {- Draft 2:
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
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 10_000

-- Properties

numAbsLaw2 ::
  forall (n :: Nat).
  (SizeNat n, 1 <= n) =>
  Property
numAbsLaw2 = forAllShrinkShow arbitrary shrink show $ \(ix1 :: Index n, ix2) ->
  property $ abs (ix1 + ix2) <= abs ix1 + abs ix2

numAbsLaw1 ::
  forall (n :: Nat).
  (SizeNat n, 1 <= n) =>
  Property
numAbsLaw1 = forAllShrinkShow arbitrary shrink show $ \(ix1 :: Index n, ix2) ->
  abs ix1 * abs ix2 === abs (ix1 * ix2)

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

-}
