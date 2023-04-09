{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Test.QuickCheck.SafeGen where

import Data.List.NonEmpty (NonEmpty (..))
import Test.QuickCheck

data Nat = Zero | Succ Nat
  deriving (Show)

infinity :: Nat
infinity = Succ infinity

safeMin :: Traversable t => t Nat -> Nat
safeMin xs = case traverse peel xs of
  Nothing -> Zero
  Just xs' -> Succ $ safeMin xs'

peel :: Nat -> Maybe Nat
peel Zero = Nothing
peel (Succ l) = Just l

data SafeGen a
  = Gen (Gen a)
  | forall i.
    Product
      { _leftArity :: Double,
        _leftBranch :: SafeGen (i -> a),
        _rightArity :: Double,
        _rightBranch :: SafeGen i
      }
  | Choice (NonEmpty (SafeGen a))

choice :: [SafeGen a] -> SafeGen a
choice [] = error "asd"
choice (a : as) =
  let ne = a :| as
   in Choice ne

cost :: SafeGen a -> Nat
cost (Gen _) = Zero
cost (Choice as) = Succ $ safeMin (cost <$> as)
