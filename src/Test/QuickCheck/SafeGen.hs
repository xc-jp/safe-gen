{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Test.QuickCheck.SafeGen
  ( runSafeGen,
    runSafeGenNoHeightCheck,
    gen,
    arb,
    SafeGen,
    oneof,
    frequency,
  )
where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

-- TODO memoize cost

data Nat = Zero | Succ Nat
  deriving (Show)

data SafeGen a
  = Gen (Gen a)
  | forall i.
    Ap
      (SafeGen (i -> a))
      (SafeGen i)
  | Choice (NonEmpty (Int, SafeGen a))

runSafeGen :: SafeGen a -> Gen a
runSafeGen sg
  | not (leqInt (shallowness sg) 20) = error "runSafeGen: Minimum depth more than 20, likely because all paths have infinite recursion!"
  | otherwise = runSafeGenNoHeightCheck sg

runSafeGenNoHeightCheck :: SafeGen a -> Gen a
runSafeGenNoHeightCheck sg0 = QC.sized (\size -> go size sg0)
  where
    go :: Int -> SafeGen a -> Gen a
    go !budget (Gen g) = QC.resize budget g
    go !budget p@Ap {} = goProduct (div budget (arity p)) p
    go !budget (Choice ((_, a) :| [])) = go budget a
    go !budget (Choice as) =
      case filter (flip leqInt budget . shallowness . snd) (toList as) of
        [] -> QC.frequency ((fmap . fmap) (go budget) (toList (safeMinBy (shallowness . snd) as)))
        as' -> QC.frequency ((fmap . fmap) (go budget) as')

    goProduct :: Int -> SafeGen a -> Gen a
    goProduct !budget (Ap l r) = goProduct budget l <*> goProduct budget r
    goProduct !budget sg = go budget sg

    arity :: SafeGen a -> Int
    arity (Ap l r) = arity l + arity r
    arity _ = 1

    safeMinBy :: Traversable t => (a -> Nat) -> t a -> NonEmpty a
    safeMinBy fcost = goMin . fmap (\x -> (x, fcost x))
      where
        unpeel (x, Zero) = VLeft (pure x)
        unpeel (x, Succ n) = VRight (x, n)
        goMin xs = case traverse unpeel xs of
          VLeft a -> a
          VRight xs' -> goMin xs'

leqInt :: Nat -> Int -> Bool
leqInt Zero !x = x > 0
leqInt (Succ n) !x = x > 1 && leqInt n (x - 1)

deriving instance Functor SafeGen

instance Applicative SafeGen where
  pure = Gen . pure
  a <*> b = Ap a b

gen :: Gen a -> SafeGen a
gen = Gen

arb :: QC.Arbitrary a => SafeGen a
arb = gen QC.arbitrary

oneof :: [SafeGen a] -> SafeGen a
oneof [] = error "SafeGen.oneof: empty list"
oneof (a : as) =
  let ne = a :| as
   in Choice ((1,) <$> ne)

frequency :: [(Int, SafeGen a)] -> SafeGen a
frequency [] = error "SafeGen.frequency: empty list"
frequency (a : as) = Choice (a :| as)

shallowness :: SafeGen a -> Nat
shallowness = go
  where
    go :: SafeGen a -> Nat
    go (Gen _) = Zero
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

data Validation e a = VLeft (NonEmpty e) | VRight a
  deriving (Functor)

instance Applicative (Validation e) where
  pure = VRight
  VRight f <*> VRight a = VRight (f a)
  VLeft e1 <*> VLeft e2 = VLeft (e1 <> e2)
  VLeft e1 <*> _ = VLeft e1
  _ <*> VLeft e2 = VLeft e2
