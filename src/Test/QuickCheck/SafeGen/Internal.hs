{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Test.QuickCheck.SafeGen.Internal where

import Control.Applicative (liftA2)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Test.QuickCheck (Arbitrary (..), Gen)
import qualified Test.QuickCheck as QuickCheck

-- | 'SafeGen' is a tree of 'Gen' values, that automatically distributes the size parameter over its branches.
-- To run/consume a tree, use 'runSafeGen'.
-- Branches are either product types (composed through the 'Applicative' interface), or sum types composed using 'oneof' or 'frequency'.
data SafeGen a
  = Gen (Gen a)
  | -- 'Pure' could be encoded as @Gen . pure@, but by special-casing it we can maintain the Applicative laws.
    -- Specifically, when dividing the size parameter, we don't count 'Pure' branches.
    Pure a
  | forall i.
    Ap
      (SafeGen (i -> a))
      (SafeGen i)
  | Choice (NonEmpty (Int, SafeGen a))

deriving instance Functor SafeGen

instance Applicative SafeGen where
  pure = Pure
  Pure a <*> b = a <$> b
  a <*> Pure b = ($ b) <$> a
  a <*> b = Ap a b

instance Arbitrary a => Arbitrary (SafeGen a) where
  arbitrary =
    QuickCheck.frequency
      [ (1, pure (Gen arbitrary)),
        (1, Pure <$> arbitrary),
        (2, liftA2 Ap (arbitrary :: Gen (SafeGen (Int -> a))) arbitrary),
        (2, Choice . fmap (\(QuickCheck.Positive (QuickCheck.Small w), g) -> (w, g)) <$> nonEmpty)
      ]
    where
      nonEmpty :: Arbitrary b => Gen (NonEmpty b)
      nonEmpty = liftA2 (:|) arbitrary arbitrary

data Nat = Zero | Succ Nat
  deriving (Show)

-- | Run a 'SafeGen' using the current context's size parameter.
-- If the 'SafeGen' value does not have a leaf within 20 layers, assume it has infinite recursion, and throw an exception.
runSafeGen :: SafeGen a -> Gen a
runSafeGen sg
  | not (leqInt (shallowness sg) 20) = error "runSafeGen: Minimum depth more than 20, likely because all paths have infinite recursion!"
  | otherwise = runSafeGenNoCheck sg

-- | like 'runSafeGen', but doesn't first check if this generator can terminate.
runSafeGenNoCheck :: SafeGen a -> Gen a
runSafeGenNoCheck sg0 = QuickCheck.sized (\size -> go size sg0)
  where
    go :: Int -> SafeGen a -> Gen a
    go _ (Pure a) = pure a
    go !size (Gen g) = QuickCheck.resize size g
    go !size p@Ap {} = goProduct (size `div` max 1 (arity p)) p
    go !size (Choice ((_, a) :| [])) = go size a
    go !size (Choice as) =
      case filter (flip leqInt size . shallowness . snd) (toList as) of
        [] -> QuickCheck.frequency ((fmap . fmap) (go size) (toList (safeMinBy (shallowness . snd) as)))
        as' -> QuickCheck.frequency ((fmap . fmap) (go size) as')

    goProduct :: Int -> SafeGen a -> Gen a
    goProduct !size (Ap l r) = goProduct size l <*> goProduct size r
    goProduct !size sg = go size sg

    arity :: SafeGen a -> Int
    arity (Ap l r) = arity l + arity r
    arity (Pure _) = 0
    arity (Gen _) = 1
    arity (Choice _) = 1

    safeMinBy :: Traversable t => (a -> Nat) -> t a -> NonEmpty a
    safeMinBy fdepth = goMin . fmap (\x -> (x, fdepth x))
      where
        unpeel (x, Zero) = VLeft (pure x)
        unpeel (x, Succ n) = VRight (x, n)
        goMin xs = case traverse unpeel xs of
          VLeft a -> a
          VRight xs' -> goMin xs'

leqInt :: Nat -> Int -> Bool
leqInt Zero !x = x > 0
leqInt (Succ n) !x = x > 1 && leqInt n (x - 1)

-- | Lift a 'Gen' generator into 'SafeGen'.
gen :: Gen a -> SafeGen a
gen = Gen

-- | Convenient synonym for 'gen arbitrary'.
arb :: Arbitrary a => SafeGen a
arb = gen arbitrary

-- | Pick one of these branches, with equal probability.
-- Only branches shallower than the current size are considered.
oneof :: [SafeGen a] -> SafeGen a
oneof [] = error "SafeGen.oneof: empty list"
oneof (a : as) = Choice $ (1,) <$> a :| as

-- | Pick one of these branches, with weighted probability.
-- Only branches shallower than the current size are considered.
frequency :: [(Int, SafeGen a)] -> SafeGen a
frequency [] = error "SafeGen.frequency: empty list"
frequency (a : as) = Choice (a :| as)

-- TODO memoize this into 'SafeGen' directly
shallowness :: SafeGen a -> Nat
shallowness = go
  where
    go :: SafeGen a -> Nat
    go (Gen _) = Zero
    go (Pure _) = Zero
    go (Choice as) = Succ $ safeMin (go . snd <$> as)
    go p@Ap {} = Succ $ goProduct p

    goProduct :: SafeGen a -> Nat
    goProduct (Ap l r) = safeMax (goProduct l) (goProduct r)
    goProduct sg = go sg

    safeMax :: Nat -> Nat -> Nat
    safeMax Zero b = b
    safeMax a Zero = a
    safeMax (Succ a) (Succ b) = Succ (safeMax a b)

    safeMin :: Traversable t => t Nat -> Nat
    safeMin xs = case traverse unsucc xs of
      Nothing -> Zero
      Just xs' -> Succ $ safeMin xs'
      where
        unsucc :: Nat -> Maybe Nat
        unsucc Zero = Nothing
        unsucc (Succ l) = Just l

-- | 'Either' that collects _all_ its failures in a list
data Validation e a = VLeft (NonEmpty e) | VRight a
  deriving (Functor)

instance Applicative (Validation e) where
  pure = VRight
  VRight f <*> VRight a = VRight (f a)
  VLeft e1 <*> VLeft e2 = VLeft (e1 <> e2)
  VLeft e1 <*> _ = VLeft e1
  _ <*> VLeft e2 = VLeft e2
