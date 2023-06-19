# Arbitrary functions in QuickCheck

## Introduction

[TODO]

## An aside about arrays

To give ourselves something to test, we will look at a small problem with a
particular data structure, and present a small solution, whose API we can
develop while testing.

### Arrays: the good and the bad

[TODO: Why arrays are good but functional ones are hard]

One option for a 'middle ground' are _pull arrays_. We can see these as an
'array builder', similarly to how text or `ByteString` builders work: we
represent an array as a combination of length and an 'index function':

```haskell
data Pull (a :: Type) = Pull Int (Int -> a)
```

Our use of `Int` here parallels that of many APIs around linear collections in
Haskell[^1]. This avoids the 'copying problem' completely. For example, consider
the `Functor` instance for this type:

```haskell
instance Functor Pull where
  fmap :: forall (a :: Type) (b :: Type) . (a -> b) -> Pull a -> Pull b
  fmap f (Pull len g) = Pull len (f . g)
```

Instead of having to potentially copy an entire array, we only need to compose
two functions. This approach works well in a pipeline similar to the following:

1. We have a lot of data to process in different ways.
2. We construct 'processing pipelines' using pull arrays, possibly re-using some
   more than once.
3. Once our computations are composed, we 'execute' the pull arrays to produce
   new data, avoiding any intermediate copies.

Furthermore, if our desired result is not another array, we have additional
options that don't result in any new arrays being produced. For example, we can
define `Foldable` for `Pull`:

```haskell
instance Foldable Pull where
  foldMap :: forall (a :: Type) (m :: Type) . Monoid m => Pull a -> m
  foldMap f (Pull len g) = foldMap (f . g) [0, 1 .. len - 1]
```

Pull arrays in the above formulation can be instances of many type classes in
addition to `Functor`, which also gives us familiar and powerful APIs we can
work with. This has led to their use in multiple places, including [at least one
Haskell library][repa].

However, pull arrays are not without their limitations: one of the biggest of
these is their inherent partiality. This stems from the 'index function' not
being aware of indices: it might be defined for some of the positions indicated
by the length parameter, but not all. While we can prevent some of these issues
by carefully hiding the constructor, doing so safely would require hiding some
useful operations. For example, consider the following:

```haskell
reindex :: forall (a :: Type) . (Int -> Int) -> Pull a -> Pull a
reindex f (Pull len g) = Pull len (g . f)
```

This takes an 'index mapping function' from result array indexes to input array
indexes, as well as an input array, producing a result whose values are
determined based on mapping each position of the result to a position in the
input. Some examples of its use are:

```haskell
-- Make an array full of the first element of arr
reindex (const 0) arr
-- Reverse arr
reindex (\ix -> length arr - 1 - ix) arr
-- Copy odd-numbered elements, but set even-numbered elements to first element
reindex (\ix -> if (ix `rem` 2 == 0) then 0 else ix) arr
```

This is a powerful primitive, which we would like to make available to our
users, but it is flawed. Not only is it not as powerful as it could be (as we
can't change the length of the array, only its contents), but it's also
dangerously partial: we don't know if our 'index mapping function' produces
sensible indexes! Worse, addressing the problem of being able change the length
of the result would require users to be even more careful to ensure they don't
access any index that doesn't make sense. On top of all this, the signature is
prone to being confusing: are we mapping result indices to input indices, or the
other way around?

### An improvement: size indexing

The main reason we see the problems of the previous section is because our
'index functions' don't operate on indexes by anything except convention. We can
resolve this problem in a similar way to how libraries like
[`vector-sized`][vector-sized] deal with out-of-bounds access: make the length a
statically-known part of the array, and instead of `Int`, use a tagged index
type. 

The idea we will use is based on [a conversation regarding indexing
safety](https://github.com/expipiplus1/vector-sized/issues/121) in the context
of `vector-sized`. At its core, it uses `GHC.TypeNats` and the `Nat` kind to
provide a type-level length, then create a type representing all indexes less
than this length. More precisely, we first define a way of restricting all
possible `Nat`s to only those that can be used as indexes on our platform:

```haskell
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
  sizeNatToInt = fromIntegral (natVal' @n proxy#)
```

This combination takes some unpacking. `SizeNat` promises two things about any
instance: firstly, that it is a genuine `Nat` (and not, for example, a stuck
type family); secondly, that its value does not exceed the maximum value of
`Int` for the platform the code was compiled on. This avoids the problems
described in the aforementioned conversation, and is why we need the Template
Haskell splice. Since only one instance of this type class makes sense, we also
provide it immediately: essentially, `sizeNatToInt` provides an `Int` equivalent
to `n`, provided it can be a sensible array length on our platform.

With this, we can define a closed type `Index`:

```haskell
newtype Index (n :: Nat) = Index Int
```

We will use `SizeNat` in subsequent sections to ensure sensible use of `Index`,
as well as providing a way to construct them conveniently. Having `Index` in
hand, we can now define the better, safer pull array:

```haskell
newtype Vector (n :: Nat) (a :: Type)
  = Vector (Index n -> a)
```

We can see two notable improvements: firstly, we no longer need to store length,
as it is explicitly defined by the type; secondly, our 'index function' no
longer takes `Int`, but instead `Index n`, which means that it is 'index-aware'.
We can still write the `Functor` instance, but it's now simpler[^2]:

```haskell
instance Functor (Vector n) where
  fmap :: forall (a :: Type) (b :: Type) . (a -> b) -> Vector n a -> Vector n b
  fmap f (Vector g) = Vector (g . f)
```

Furthermore, not only can we have a fully length-general `reindex`, but it is
also 100% safe:

```haskell
reindex ::
  forall (n :: Nat) (m :: Nat) (a :: Type).
  (Index n -> Index m) ->
  Vector m a ->
  Vector n a
reindex f (Vector g) = Vector $ g . f
```

As a bonus, the type of our improved `reindex` clearly designates which indices
we are mapping from and to, making it easier for our users.

With all of these in hand, let's see how QuickCheck can help us ensure our APIs
are correct.

## QuickCheck basics for `Index` correctness

### First properties

Currently, `Index` isn't very useful as a type. Users have no way to construct
values of it, and even if they were to have a value somehow, there's nothing
they can do with it. At minimum, we want to be able to do the following:

1. Compare `Index`es for equality and order;
2. Work with `Index`es as if they were numbers, especially with regard to number
  literal syntax; and
3. Enumerate `Index`es.

We can do the first of these with some derivations based on the underlying
`Int`. We'll also derive `Show` for future debugging purposes[^3].

```haskell
deriving stock instance Show (Index n)

deriving via Int instance Eq (Index n)

deriving via Int instance Ord (Index n)
```

For the second, we first need to decide what exactly we want indexes to do as
numbers. A natural, and convenient, option is to treat `Index n` as natural
numbers modulo `n`. This gives us almost everything we need for the `Num` type
class[^4]: the only remaining decision is around `fromInteger`, as this will
determine what literal syntax will do. Unfortunately, `fromInteger` overloads
syntax for _all_ integers, including negative ones. To make this work, and
arguably to borrow a useful feature, we will use the 'wrap-around' behaviour of
Python list indexes: thus, `-1` will mean 'last valid position', `-2`
will mean 'second-to-last valid position' etc. While this will allow very large
wrap-arounds, it means we can avoid partiality. Here is our attempt at
implementation:

```haskell
-- This has a bug!
instance (1 <= n, SizeNat n) => Num (Index n) where
  Index x + Index y = Index $ (x + y) `rem` sizeNatToInt @n
  Index x * Index y = Index $ (x * y) `rem` sizeNatToInt @n
  negate (Index x) =
    if x == 0
      then Index 0
      else Index $ sizeNatToInt @n - x
  abs = id
  signum ix = min ix (Index 1)
  fromInteger x = case signum x of
    (-1) -> fromInteger . negate $ x
    0 -> Index 0
    _ -> Index . fromIntegral $ x `rem` fromIntegral (sizeNatToInt @n)
```

We define `negate` instead of `-`: effectively, the additive inverse of an index
is its 'mirror twin' reflected about 0. For example, given
the values of `Index 5`, we would have:

* `negate 0 = 0`
* `negate 1 = 4`
* `negate 2 = 3`
* `negate 3 = 2`
* `negate 4 = 1`

We use a similar idea for `fromInteger`. This gives us the Pythonesque literal
behaviour for indexes. However, can we be sure this is correct? The
functionality is not trivial, and in fact contains a mistake already!

We will now use QuickCheck to verify our instance. Our first step is to think of
a property to test: some kind of equational relationship that QuickCheck will
try and find a counter-example for. For a type class, the starting point should
be its laws: however, `Num` technically [doesn't have
any](https://hackage.haskell.org/package/base-4.18.0.0/docs/GHC-Num.html#t:Num),
and in particular, since we don't have an instance of `Integral`, even the
suggested laws don't help us with regard to `fromInteger`. However, if we assume
that what we're defining with `instance Num (Index n)` are [integers modulo
`n`](https://en.wikipedia.org/wiki/Modular_arithmetic#Integers_modulo_n), we
expect `fromInteger` to be the [ring homomorphism][ring-homomorphism] from
`Integer` to `Index n`. This gives us three laws:

* `fromInteger i + fromInteger j = fromInteger (i + j)`
* `fromInteger i * fromInteger j = fromInteger (i * j)`
* `fromInteger 1 = 1`

We will focus on the first two of these. To begin with, we need to provide a way
for QuickCheck to work with `Index`es, by way of an `Arbitrary` instance. This
gives us two capabilities: a way of pseudorandomly generating `Index`es (a
_generator_), and a way of taking an `Index` which is part of a 
counter-example and 'simplifying' it (a _shrinker_). The shrinker in particular
is crucial for QuickCheck to be useful - without one, counter-examples can
easily end up too complex for us to even make sense of, much less diagnose.
Additionally, we must ensure that:

* The generator is _fair_: we should not bias ourselves in what values of
  `Index` we can generate.
* The shrinker is _terminating_: we have at least one value which is seen as
  'the simplest' and no longer shrinks.

A suitable instance would be:

```haskell
instance (1 <= n, SizeNat n) => Arbitrary (Index n) where
  arbitrary :: Gen (Index n)
  arbitrary = Index <$> chooseInt (0, sizeNatToInt @n - 1)
  shrink :: Index n -> [Index n]
  shrink (Index ix) = do
    ix' <- shrink ix
    guard (ix' < 0)
    pure . Index $ ix'
```

The generator uses the builtin `chooseInt`, which is both efficient for the type
we need, and also fair in the given (inclusive) range. We effectively 'borrow'
a generator for `Int`, but impose additional constraints on it, based on the
`SizeNat` type class, to ensure the internal invariants of `Index` are
preserved. The shrinker is worth
further examination. According to its type, it takes an `Index n` and produces a
_list_ of `Index n`s: rather than producing a literal linear collection, we view
it as a stream of 'simpler' values than the input, or, looking at it another
way, as a nondeterministic search for such values. In this sense, our use of
`guard` makes sense here: it specifies that if we 'shrink into' an `Int` than is
below `0`, we should cut off that search branch. As with the generator, we
'borrow' the underlying behaviour of `Int`, but constrain it to ensure we don't
violate the invariants of `Index`.

Armed with this instance, let us try to verify the property `fromInteger i +
fromInteger j = fromInteger (i + j)`. To do this, we will use [`tasty`][tasty]
and [`tasty-quickcheck`][tasty-quickcheck]:

```haskell
main :: IO ()
main = 
  defaultMain . testGroup "Properties" $
    [ testGroup
        "Index"
        [ testProperty "fromInteger i + fromInteger j = fromInteger (i + j)" $ fiProp1 @1000
        ]
    ]

fiProp1 ::
  forall (n :: Nat).
  (SizeNat n, 1 <= n) =>
  Property
fiProp1 = forAllShrinkShow arbitrary shrink show $ \(i, j) ->
  fromInteger i + fromInteger j === fromInteger @(Index n) (i + j)
```

This is a standard pattern for setting up a property test in `tasty`: define a
top-level group, define (or use) a sub-group per type, then use `testProperty`
to specify what we want checked. The property implementation is almost a
word-for-word translation, but makes use of a few important techniques[^5]:

* We use `===`, which relies on `Eq` to verify the property holds.
* We use `forAllShrinkShow`, then provide explicit (if `Arbitrary` and
  `Show`-based in this case) implementations of a generator, shrinker and debug
  visualizer.
* We generate two values at once, by using the derived `Arbitrary` instance for
  pairs.

While not strictly necessary in this case, it is a good idea to use
`forAllShrinkShow` and explicit functions for generation, shrinking and showing,
for reasons that will become apparent in future sections. It also allows writing
tests against types without `Arbitrary` instances, which can be useful if you
want to avoid orphan instances, for example.

Before running anything, we want to ensure we get the best output, and the
fastest tests, we can. Firstly, we need to make sure we're using direct output:
the easiest method for this is adding the following to your `cabal.project`:

```
test-show-details: direct
```

This ensures that we get output both on success and on failure, and we receive
the output as the tests complete, not all at once at the end. Secondly, we want
to ensure tests are being run in parallel: the easiest way to achieve this is to
add the following entry to the `test-suite` stanza in the Cabal file:

```
  ghc-options:    -threaded -rtsopts -with-rtsopts=-N
```

This will compile our tests to enable parallel execution, and will run with as
much parallelism as our system permits.

With this done, we run our tests with `cabal test`, and see the following:

```
Properties
  Index
    fromInteger i + fromInteger j = fromInteger (i + j): FAIL
      *** Failed! Falsified (after 4 tests and 3 shrinks):
      (-1,1)
      Index 2 /= Index 0
      Use --quickcheck-replay=180885 to reproduce.
```

QuickCheck has found this issue after a small number of tests, but also
simplified it as much as it could. However, even with this, it's difficult to
understand what exactly we're being told here: on the one hand, we see a pair of
`(-1, 1)`, but on the other hand, the failure of the test was due to `Index 2 /=
Index 0`. It is difficult to see the relationship between these, and while in
this case, we can probably infer what needs fixing, for more complex cases, this
can be a serious issue. 

Before we go any further and attempt to fix this breakage, let's make the output
more readable. Luckily for us, QuickCheck provides us with a way of reproducing
the exact same sequence of events that led to the failure we saw, by way of
`--quickcheck-replay`. This is useful in other contexts as well, especially on
collaborative projects where not everyone may know enough to fix an issue, as it
makes reproducing a case easy. 

There are two issues with the output: firstly, while we _generate_ a pair, we
never actually use it as such; secondly, it's not clear how our inputs translate
into the failing case. This fixes both issues:

```haskell
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
```

This is an example of where `forAllShrinkShow` shines: it allows us to display
our generated values in a way that is easier to understand in the context of the
test. We use this to 'name' the generated values relative the test's own name.
Secondly, we make use of `counterexample`: this is a 'property modifier'
function that takes a `String` and a property, and produces a new property where
the `String` is added to the output in case it fails. Here, we show exactly what
`Index`es we get from our arguments, once again using their 'names' from the
test's name. With this, we can present failures in a way that's easier for a
human to understand. This is what we see if we re-run the case with `cabal test
--test-option=--quickcheck-replay=180885`:

```
Properties
  Index
    fromInteger i + fromInteger j = fromInteger (i + j): FAIL
      *** Failed! Falsified (after 4 tests and 3 shrinks):
      i = -1, j = 1
      fromInteger i = Index 1
      fromInteger j = Index 1
      Index 2 /= Index 0
      Use --quickcheck-replay=180885 to reproduce.
```

Much better! We can now see clearly what's happening here: we're not dealing
correctly with negative `Integer`s: in particular, `-1` as a literal does not
refer to the last index as it should. Looking at our implementation of
`fromInteger`, we can fix this problem as follows:

```haskell
  fromInteger x = case signum x of
      -- This line is the key
      (-1) -> negate . fromInteger . negate $ x
      0 -> minBound
      _ -> Index . fromIntegral $ x `rem` fromIntegral (sizeNatToInt @n)
```

This essentially performs a 'double reflection': first, we 'reflect' the
`Integer` input about `0`, then convert it to an `Index`, then 'reflect'
again[^6]. We missed that second 'reflection' in our initial attempt. Now,
verifying whether this fixes our implementation using `cabal test
--test-option=--quickcheck-replay=180885`:

```
Properties
  Index
    fromInteger i + fromInteger j = fromInteger (i + j): OK
      +++ OK, passed 100 tests.
```

It seems that our problem is resolved. However, we can't be sure of this: the
next section will explain why, and what we can do about it.

### Running more, running better

According to the output of our previous property test, QuickCheck ran 100 tests.
While better than no tests at
all, 100 tests isn't a very meaningful number. Consider our property definition:
we ask for two arbitrary `Integer`s. How much coverage of our possibilities
could 100 tests provide? With so few tests, we can't be sure that we're not
getting a _false negative_: we pass (that is, find no counter-examples) not
because none exist, but because we didn't look hard enough[^7]!

While it isn't generally possible to prevent false negatives of this kind (as
this would require an exhaustive search), we can make it less likely by running
more tests. `tasty` makes this straightforward:

```haskell
main :: IO ()
main =
  defaultMain . adjustOption moreTests . testGroup "Properties" $
    [ testGroup
        "Index"
        [ testProperty "fromInteger i + fromInteger j = fromInteger (i + j)" $ fiProp1 @1000
        ]
    ]
  where
    moreTests :: QuickCheckTests -> QuickCheckTests
    moreTests = max 10_000
```

Here, we are making use of `tasty`'s options: individual adjustments to
different kinds of tests, that can be applied to anything ranging from an
individual test, to a whole group of such. `adjustOption` modifies an option for
those tests it is applied to: here, we are modifying `QuickCheckTests`, provided
by `tasty-quickcheck`. Using `adjustOption` in this way forces at least 10,000
tests to run, which makes false negatives much less likely:

```
Properties
  Index
    fromInteger i + fromInteger j = fromInteger (i + j): OK (0.02s)
      +++ OK, passed 10000 tests.
```

However, if we want to run more tests, we can request that via the command-line
API. For example, if we wanted 20,000 tests instead, we would use `cabal test
--test-option=--quickcheck-tests=20000`:

```
Properties
  Index
    fromInteger i + fromInteger j = fromInteger (i + j): OK (0.03s)
      +++ OK, passed 20000 tests.
```

This way, we simultaneously ensure a reasonable number of tests, but don't
prevent more if someone wants them. 

This raises an interesting question: how many tests is 'enough'? In general,
there's no good answer to this question. While in theory, 'as many as possible'
is a valid answer, it isn't very practical, as it can make tests take a very
long time, and tests that run slowly often become tests than _never_ run
(because everyone skips them). Furthermore, in the context of CI, longer-running
tests mean longer development cycles. Lastly, if your test's values come from a
truly finite type, there is definitely an upper limit to the number of useful
tests, as beyond that, all you're doing is re-running cases. Generally, we
recommend:

* For large (or infinite) types, run at least tens of thousands of tests.
* For small types, run between 3 and 4 times the type's size in tests.

We found this to be a good compromise between avoiding false negatives and
keeping tests fast.

### Testing `Vector`, introducing `CoArbitrary`

So far, we have used QuickCheck for tests around concrete, non-function,
non-higher-kinded values; in this form, QuickCheck is familiar to many. However,
QuickCheck is able to work with higher-kinded polymorphic types, and even
functions.

To demonstrate, let's consider `reindex` again. If it is implemented correctly,
we expect the following to hold[^8]:

```
reindex f . reindex g = reindex (g . f)
```

To see why, suppose `f :: Index n1 -> Index n2` and `g :: Index n2 -> Index n3`.
Thus, `reindex f . reindex g` is like first constructing an array indexed by
`Index n2`, then using it to construct another array indexed by `Index n1`. We
also see that `g . f :: Index n1 -> Index n3`: essentially, we 'bypass' the
intermediate array completely by computing indexes directly from the result that
we want, instead of having an intermediate 'lookup table'. We would like to test
this property, not only because of the flexibility of this function, but also to
prevent any regressions (for example, on a [representation
change][defun-push-array]). A direct attempt to write such a property would look
something like this:

```haskell
-- Will not work
reindexProp ::
  forall (n1 :: Nat) (n2 :: Nat) (n3 :: Nat).
  (SizeNat n1, 1 <= n1, SizeNat n2, 1 <= n2, SizeNat n3, 1 <= n3) =>
  Property
reindexProp = forAllShrinkShow arbitrary shrink show $
  \(f :: Index n1 -> Index n2, g :: Index n2 -> Index n3, v) ->
    (reindex f . reindex g $ v) === reindex (g . f) v
```

We hit a roadblock almost immediately though: what should the type of `v`'s
elements be? While we could pick a concrete type, this feels wrong: it should
not matter at all what exact type is in a `Vector` as far as `reindex` is
concerned. This is a common case for 'structural' operations on linear
collections: if the function is meant to work over the 'structure' of the
collection, the type of its contents shouldn't be relevant.

To this end, QuickCheck provides the types `A`, `B` and `C` in
`Test.QuickCheck.Poly`. These are supposed to represent type parameters that are
not relevant to the property, to 'fill in' any polymorphic type arguments. These
types have only the minimal necessary capability to enable QuickCheck to work
with them, but nothing else: by using them, we simulate a parametric case:

```haskell
-- Still will not work, but closer
reindexProp ::
  forall (n1 :: Nat) (n2 :: Nat) (n3 :: Nat).
  (SizeNat n1, 1 <= n1, SizeNat n2, 1 <= n2, SizeNat n3, 1 <= n3) =>
  Property
reindexProp = forAllShrinkShow arbitrary shrink show $
  \(f :: Index n1 -> Index n2, g :: Index n2 -> Index n3, v :: Vector n3 A) ->
    (reindex f . reindex g $ v) === reindex (g . f) v
```

This leads to GHC emitting an error like this:

```
Could not deduce (Arbitrary (Vector n3 A)) arising from a use of ‘arbitrary’
```

This is not surprising, as we haven't defined an `Arbitrary` instance for
`Vector`. However, if we try, we can't even get off the ground:

```haskell
instance Arbitrary a => Arbitrary (Vector n a) where
  arbitrary = Vector <$> _ -- what possibly goes here?
```

To fill that hole, we need something whose type is `Gen (Index n -> a)`.
However, how would we obtain such a thing? Arbitrarily-generating a _function_
feels like a completely different problem to generating a non-function value. If
we soldier on, and see if QuickCheck has some solution, we are even more
confused:

```haskell
instance Arbitrary a => Arbitrary (Vector n a) where
  arbitrary = Vector <$> arbitrary
  -- Gives an error similar to this:
  -- Could not deduce (Test.QuickCheck.Arbitrary.CoArbitrary (Index n))
```

What is `CoArbitrary` and why do we need it? Looking at its definition:

```haskell
class CoArbitrary (a :: Type) where
  coarbitrary :: forall (b :: Type) . a -> Gen b -> Gen b
```

The intent of `coarbitrary` becomes more clear if we place the implicit
parentheses back into its type: `forall (b :: Type) . a -> (Gen b -> Gen b)`.
Essentially, this says that, given a value of type `a`, for any other type `b`,
we can 'perturb' or 'affect' a generator for `b` based on a value of `a`. This
enables QuickCheck to generate arbitrary functions:

```haskell
instance (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)
```

Essentially, to generate a function of type `a -> b`, we use the function's
input to 'perturb' the generator for the type of the function's output. This
way, arguments affect the output of the function, but the results for any given
input are still pseudorandom. Notably, values of type `a` are used in a
read-only manner: we don't have to worry about invariants.

To define an instance of `CoArbitrary` for `Index n`, we can 'borrow' the
`CoArbitrary` instance for `Int`:

```
instance CoArbitrary (Index n) where
  coarbitrary (Index i) = coarbitrary i
```

This is safe, as even though `Index` has internal invariants, we use its
representation in a read-only manner. With this, we can define `arbitrary` for
`Vector`, but `shrink` is a problem:

```haskell
-- Compiles, but doesn't do what you think!
instance Arbitrary a => Arbitrary (Vector n a) where
  arbitrary = Vector <$> arbitrary
  shrink (Vector f) = Vector <$> shrink f
```

The problem here is that `shrink` is not a mandatory method of `Arbitrary`, and
some instances leave it out: in our particular case, the instance for `Arbitrary
(a -> b)` [does
this](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/src/Test.QuickCheck.Arbitrary.html#line-422). 
If omitted, the method is equivalent to 'do not shrink'. This is a problem, as
without a shrinker, QuickCheck has no way to simplify counter-examples. As we
have seen before, this is an important capability of QuickCheck, and giving it
up makes it much more difficult for us. We need a different technique to solve
this problem.

### Better functions with `Function`

The problem of shrinking of generated function (as well as some others) led to
the development of a different method of generation, as described in [_Shrinking
and showing functions_][shrinking-and-showing]. The method described there
relies on a different type class:

```haskell
class Function (a :: Type) where
  function :: forall (c :: Type) . (a -> c) -> a :-> c
```

We can think of `a :-> c` as an _explicit partial function_. More exactly, `a
:-> c` is represented as a table of pairs of `a` and `c` (that is, input and
output): in this sense, it is _explicit_[^9]. However, instead of generating the
entire table up-front (which may not even be possible if `a` is infinite), the
table is produced on demand: in this sense, it is _partial_, as it's not
necessarily defined everywhere. Whenever we need an output we don't have a
pairing for, we generate one, but if we already have a pairing in the table, we
re-use it. 

This representation enables shrinking, as we can shrink the table representing
the function. However, it also enables other capabilities, most notably `show`.
It is also compositional: we can use this representation to construct instances
for functions of multiple arguments conveniently, for example.

The `function` method essentially enables the construction of this tabular
representation: given a way of taking an `a` to any other type `c`, this is used
as a basis for the explicit partial function. Specifically, the provided mapping
`a -> c` is used to build 'table entries' on demand. As with `coarbitrary`,
`function` uses `a` in a read-only manner only, meaning that we don't need to
worry about internal invariants on `a`'s values.

Similarly to the `coarbitrary` implementation for `Index`, we can re-use the
`Function` instance for `Int` to implement `function` for `Index`. However, we
can't do this as easily as for `coarbitrary`, as `:->` is a closed type.
Instead, we have to use a helper `functionMap`:

```haskell
functionMap :: forall (b :: Type) (a :: Type) (c :: Type) .
  Function b =>
  (a -> b) -> (b -> a) -> (a -> c) -> a :-> c
```

`functionMap` requires two arguments to construct a valid definition of
`function` for a type of our choice: a way to transform the type we want an
instance for (`a` in this example) to another type which already has an instance
(`b` in this example), and a way to 'undo' that transformation. Put another way,
for `functionMap f g`, we expect that `f . g = g . f = id`. Using `functionMap`,
we can 'borrow' the `Function` instance of `b`, but use it for `a`. To make this
clearer, let's specialize `functionMap` for `Index n`, as well as restoring some
implicit parentheses:

```haskell
functionMap :: forall (b :: Type) (n :: Nat) (c :: Type) . 
  Function b =>
  (Index n -> b) -> (b -> Index n) -> ((Index n -> c) -> Index n :-> c)
```

[TODO: Complete this]

[shrinking-and-showing]: https://dl.acm.org/doi/10.1145/2430532.2364516
[defun-push-array]: https://www.researchgate.net/publication/266661255_Defunctionalizing_Push_arrays
[tasty]: https://hackage.haskell.org/package/tasty
[tasty-quickcheck]: https://hackage.haskell.org/package/tasty-quickcheck
[ring-homomorphism]: https://en.wikipedia.org/wiki/Ring_homomorphism
[repa]: https://hackage.haskell.org/package/repa
[vector-sized]: https://hackage.haskell.org/package/vector-sized
[^1]: Even though this is arguably not a good idea: it doesn't parallel C, nad
    creates a lot of issues with partiality.
[^2]: You can also do something like `deriving via ((->) (Index n)) instance
    Functor (Vector n)`, since we have a `newtype` now. Probably not useful
    here, but can be used for other useful instances.
[^3]: We don't need standalone or via derivations here: we could have used the
    regular deriving mechanism too.
[^4]: A better, and easier, approach would be to use `Semiring` from
    [`semirings`](https://hackage.haskell.org/package/semirings), but it won't
    add new capabilities, and `Num` is more familiar.
[^5]: Worth noting as well that you should _always_ import QuickCheck-related
    identifiers from QuickCheck itself, not `tasty-quickcheck`, as that way, you
    know exactly what version you are getting. This is an [ongoing
    issue](https://github.com/UnkindPartition/tasty/issues/208).
[^6]: Arguably this is inefficient: we don't need to do two reflections.
    However, for our purposes, this is good enough.
[^7]: This is a strong counter-argument to the use of Template Haskell to 'pick
    up' QuickCheck properties from your codebase: in addition to being soupy and
    requiring [odd
    practices](https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck.html#v:quickCheckAll),
    we have _zero_ control over how many tests QuickCheck will run.
[^8]: This is secretly [contravariant
    fusion](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Functor-Contravariant.html#t:Contravariant).
[^9]: This is in some ways similar to the set-theoretical representation of
    (unary) functions as a set of input-output pairs.
