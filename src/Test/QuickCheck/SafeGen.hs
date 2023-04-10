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

-- TODO rename
safeMin :: Traversable t => t Nat -> Nat
safeMin xs = case traverse unsucc xs of
  Nothing -> Zero
  Just xs' -> Succ $ safeMin xs'
  where
    unsucc :: Nat -> Maybe Nat
    unsucc Zero = Nothing
    unsucc (Succ l) = Just l

-- TODO rename
safeMinCost :: Traversable t => (a -> Nat) -> t a -> NonEmpty a
safeMinCost fcost = go . fmap (\x -> (x, fcost x))
  where
    unpeel (x, Zero) = VLeft (pure x)
    unpeel (x, Succ n) = VRight (x, n)
    go xs = case traverse unpeel xs of
      VLeft a -> a
      VRight xs' -> go xs'

data SafeGen a
  = Gen (Gen a)
  | forall i.
    Product
      (SafeGen (i -> a))
      (SafeGen i)
  | Choice (NonEmpty (Int, SafeGen a))

runSafeGen :: SafeGen a -> Gen a
runSafeGen sg
  | not (leqInt (cost sg) 20) = error "runSafeGen: Minimum depth more than 20, likely because all paths have infinite recursion!"
  | otherwise = runSafeGenNoHeightCheck sg

runSafeGenNoHeightCheck :: SafeGen a -> Gen a
runSafeGenNoHeightCheck sg0 = QC.sized (\size -> go size sg0)
  where
    go :: Int -> SafeGen a -> Gen a
    go !budget (Gen g) = QC.resize budget g
    go !budget p@Product {} = goProduct (div budget (arity p)) p
    go !budget (Choice ((_, a) :| [])) = go budget a
    go !budget (Choice as) =
      case filter (flip leqInt budget . cost . snd) (toList as) of
        [] -> QC.frequency ((fmap . fmap) (go budget) (toList (safeMinCost (cost . snd) as)))
        as' -> QC.frequency ((fmap . fmap) (go budget) as')

    goProduct :: Int -> SafeGen a -> Gen a
    goProduct !budget (Product l r) = goProduct budget l <*> goProduct budget r
    goProduct !budget sg = go budget sg

    arity :: SafeGen a -> Int
    arity (Product l r) = arity l + arity r
    arity _ = 1

leqInt :: Nat -> Int -> Bool
leqInt Zero !x = x > 0
leqInt (Succ n) !x = x > 1 && leqInt n (x - 1)

deriving instance Functor SafeGen

instance Applicative SafeGen where
  pure = Gen . pure
  a <*> b = Product a b

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

cost :: SafeGen a -> Nat
cost (Gen _) = Zero
cost (Choice as) = Succ $ safeMin (cost . snd <$> as)
cost (Product l r) = Succ $ cost l `addNat` cost r
  where
    addNat :: Nat -> Nat -> Nat
    addNat Zero b = b
    addNat (Succ a) b = Succ (addNat a b)

data Validation e a = VLeft (NonEmpty e) | VRight a
  deriving (Functor)

instance Applicative (Validation e) where
  pure = VRight
  VRight f <*> VRight a = VRight (f a)
  VLeft e1 <*> VLeft e2 = VLeft (e1 <> e2)
  VLeft e1 <*> _ = VLeft e1
  _ <*> VLeft e2 = VLeft e2
