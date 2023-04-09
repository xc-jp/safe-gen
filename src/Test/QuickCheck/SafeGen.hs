{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.QuickCheck.SafeGen
  ( choice,
    runSafeGen,
    gen,
    SafeGen,
  )
where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Test.QuickCheck

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
safeMinCost :: Traversable t => t (SafeGen a) -> NonEmpty (SafeGen a)
safeMinCost = go . fmap (\x -> (x, cost x))
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
      { _leftArity :: !Double,
        _leftBranch :: SafeGen (i -> a),
        _rightArity :: !Double,
        _rightBranch :: SafeGen i
      }
  | Choice (NonEmpty (SafeGen a))

runSafeGen :: SafeGen a -> Gen a
runSafeGen sg = sized (\size -> go (fromIntegral size) sg)
  where
    go :: Double -> SafeGen a -> Gen a
    go budget (Gen g) = resize (floor budget) g
    go budget (Product al l ar r) = let a = al + ar in go (budget * al / a) l <*> go (budget * ar / a) r
    go budget (Choice as) = case filter (flip leqDouble budget . cost) (toList as) of
      [] -> oneof (go budget <$> toList (safeMinCost as))
      as' -> oneof (go budget <$> as')

leqDouble :: Nat -> Double -> Bool
leqDouble Zero !x = x > 0
leqDouble (Succ n) !x = x > 1 && leqDouble n (x - 1)

deriving instance Functor SafeGen

instance Applicative SafeGen where
  pure = Gen . pure
  a <*> b = Product (arity a) a (arity b) b
    where
      arity :: SafeGen q -> Double
      arity (Product l _ r _) = l + r
      arity _ = 1

gen :: Gen a -> SafeGen a
gen = Gen

choice :: [SafeGen a] -> SafeGen a
choice [] = error "SafeGen.choice: empty list"
choice (a : as) =
  let ne = a :| as
   in Choice ne

cost :: SafeGen a -> Nat
cost (Gen _) = Succ Zero
cost (Choice as) = safeMin (cost <$> as)
cost (Product _ l _ r) = cost l `addNat` cost r
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
