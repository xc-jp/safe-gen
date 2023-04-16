{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.QuickCheck.SafeGen.Internal where

import Control.Applicative (liftA2)
import Data.List.NonEmpty (NonEmpty (..))
import Test.QuickCheck

-- 'Pure' could be encoded as @Gen . pure@, but by special-casing it we can maintain the Applicative laws.
-- Specifically, when dividing the size parameter, we don't count 'Pure' branches.
data SafeGen a
  = Gen (Gen a)
  | Pure a
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
    frequency
      [ (1, pure (Gen arbitrary)),
        (1, Pure <$> arbitrary),
        (2, liftA2 Ap (arbitrary :: Gen (SafeGen (Int -> a))) arbitrary),
        (2, Choice . fmap (\(Positive (Small w), g) -> (w, g)) <$> nonEmpty)
      ]
    where
      nonEmpty :: Arbitrary b => Gen (NonEmpty b)
      nonEmpty = liftA2 (:|) arbitrary arbitrary
