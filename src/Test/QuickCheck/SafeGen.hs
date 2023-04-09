{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Test.QuickCheck.SafeGen where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Test.QuickCheck

data Nat = Strict !Word | Succ Nat
  deriving (Show)

infinity :: Nat
infinity = Succ infinity

toInt :: Nat -> Word
toInt (Strict n) = n
toInt (Succ l) = 1 + toInt l

minSeq :: Word -> Nat -> Word
minSeq n = go 0
  where
    go !s (Strict l)
      | s + l >= n = n
      | otherwise = s + l
    go !s (Succ l)
      | s >= n = n
      | otherwise = go (s + 1) l

force :: Nat -> Word
force = go 0
  where
    go !s (Strict w) = s + w
    go !s (Succ l) = go (s + 1) l

minSeqs :: [Nat] -> Word
minSeqs xs = n + foldr (flip minSeq) w xs'
  where
    (w, n, xs') = go xs 0
    peel (Strict w) = Left w
    peel (Succ l) = Right l
    go :: [Nat] -> Word -> (Word, Word, [Nat])
    go xs n = case traverse peel xs of
      Left w -> (w, n, xs)
      Right xs' -> go xs' (n + 1)

data SafeGen a
  = Gen (Gen a)
  | forall i. Product Double (SafeGen (i -> a)) Double (SafeGen i)
  | Choice (NonEmpty (SafeGen a))

choice :: [SafeGen a] -> SafeGen a
choice [] = error "asd"
choice (a : as) =
  let ne = a :| as
   in Choice ne

loops = choice [loops, choice [Gen undefined]]

terminates = choice [terminates, Gen undefined]

cost :: SafeGen a -> Nat
cost (Gen _) = Strict 0
cost (Choice as) = Strict $ minSeqs (cost <$> toList as)

-- cost (Product  _ l _ r) = S $

-- runGen :: Double -> SafeGen a -> Gen a
-- runGen budget (Gen a) = a
-- runGen budget (Product rl gl rr gr) = a
