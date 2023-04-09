{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Test.QuickCheck.SafeGen where

-- module Test.QuickCheck.SafeGen
--   ( runSafeGen,
--     gen,
--     SafeGen,
--     oneof,
--     frequency,
--   )
-- where

import Control.Applicative
import Control.Monad
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Stack (HasCallStack)
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
      { _leftArity :: !Double,
        _leftBranch :: SafeGen (i -> a),
        _rightArity :: !Double,
        _rightBranch :: SafeGen i
      }
  | Choice (NonEmpty (Int, SafeGen a))

runSafeGen :: SafeGen a -> Gen a
runSafeGen sg
  | not (leqInt (cost sg) 20) = error "runSafeGen: Minimum tree height is more than 100, you most likely have guarantueed infinite recursion!"
  | otherwise = runSafeGenNoHeightCheck sg

runSafeGenNoHeightCheck :: SafeGen a -> Gen a
runSafeGenNoHeightCheck sg = QC.sized (\size -> go (fromIntegral size) sg)
  where
    go :: Double -> SafeGen a -> Gen a
    go budget (Gen g) = QC.resize (floor budget) g
    go budget (Product al l ar r) = let a = al + ar in go (budget * al / a) l <*> go (budget * ar / a) r
    go budget (Choice as) = case filter (flip leqDouble budget . cost . snd) (toList as) of
      [] -> QC.frequency ((fmap . fmap) (go budget) (toList (safeMinCost (cost . snd) as)))
      as' -> QC.frequency ((fmap . fmap) (go budget) as')

-- goProduct :: Int -> SafeGen a -> Gen a
-- goProduct budget = undefined

-- arity :: SafeGen a -> Int
-- arity (Product l r) = arity l + arity r

leqDouble :: Nat -> Double -> Bool
leqDouble Zero !x = x > 0
leqDouble (Succ n) !x = x > 1 && leqDouble n (x - 1)

leqInt :: Nat -> Int -> Bool
leqInt Zero !x = x > 0
leqInt (Succ n) !x = x > 1 && leqInt n (x - 1)

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
cost (Product _ l _ r) = Succ $ cost l `addNat` cost r
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

data Trie a = Leaf a | Branch (Trie a) (Trie a) (Trie a)
  deriving (Show, Foldable)

safeGenTrie :: SafeGen (Trie ())
safeGenTrie =
  oneof
    [ pure (Leaf ()),
      liftA3 Branch safeGenTrie safeGenTrie safeGenTrie
    ]

instance QC.Arbitrary a => QC.Arbitrary (Trie a) where
  -- arbitrary = QC.oneof [Leaf <$> QC.arbitrary, liftM3 Branch QC.arbitrary QC.arbitrary QC.arbitrary]
  arbitrary = runSafeGen go
    where
      go = oneof [Leaf <$> gen QC.arbitrary, liftA3 Branch go go go]
