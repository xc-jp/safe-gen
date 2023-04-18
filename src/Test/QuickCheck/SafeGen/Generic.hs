{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.QuickCheck.SafeGen.Generic
  ( SafeArbitrary (..),
    FromSafeArbitrary (..),
  )
where

import Control.Applicative (liftA2)
import qualified Control.Applicative
import qualified Control.Monad.Identity as Control.Monad
import Data.Coerce
import qualified Data.Complex
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Monoid
import qualified Data.Ratio
import qualified Foreign
import qualified Foreign.C
import GHC.Generics
import Test.QuickCheck.Arbitrary
import qualified Test.QuickCheck.Modifiers
import Test.QuickCheck.SafeGen.Internal

-- | Like 'Arbitrary', but with 'SafeGen' instead of 'Gen'.
-- In practice, you probably won't interface with this class directly other than deriving an instance when deriving 'Arbitrary' via 'FromSafeArbitrary'.
--
-- One exception might be when you're not happy with the way 'FromSafeArbitrary' derives 'shrink', and you want to manually implement it.
-- In that case, you can manually write @'arbitrary' = 'runSafeGen' 'safeArbitrary'@
--
-- Another example is when working with lazy infinite data, in which case you might want to remove the termination check using @'arbitrary' = 'runSafeGenNoCheck' 'safeArbitrary'@.
class SafeArbitrary a where
  safeArbitrary :: SafeGen a
  default safeArbitrary :: (Generic a, GSafeArbitrary (Rep a)) => SafeGen a
  safeArbitrary = to <$> gsafeArbitrary

class GSafeArbitrary a where
  gsafeArbitrary :: SafeGen (a x)

class GSafeArbitrarySum a where
  gsafeArbitrarySum :: NonEmpty (Int, SafeGen (a x))

-- | Intended to be used as a deriving conduit for 'Arbitrary' from 'SafeArbitrary'.
-- This defines 'arbitrary' as @'runSafeGen' 'arbitrary'@, and 'shrink' as 'genericShrink'.
newtype FromSafeArbitrary a = FromSafeArbitrary a

instance
  ( Generic a,
    SafeArbitrary a,
    RecursivelyShrink (Rep a),
    GSubterms (Rep a) a
  ) =>
  Arbitrary (FromSafeArbitrary a)
  where
  arbitrary = FromSafeArbitrary <$> runSafeGen safeArbitrary
  shrink (FromSafeArbitrary a) = FromSafeArbitrary <$> genericShrink a

instance GSafeArbitrarySum f => GSafeArbitrary (M1 D c f) where
  gsafeArbitrary :: forall x. SafeGen (M1 D c f x)
  gsafeArbitrary = Choice $ coerce (gsafeArbitrarySum :: NonEmpty (Int, SafeGen (f x)))

instance (GSafeArbitrarySum f, GSafeArbitrarySum g) => GSafeArbitrarySum (f :+: g) where
  gsafeArbitrarySum :: forall x. NonEmpty (Int, SafeGen ((f :+: g) x))
  gsafeArbitrarySum = (fmap . fmap . fmap) L1 gsafeArbitrarySum <> (fmap . fmap . fmap) R1 gsafeArbitrarySum

instance GSafeArbitrary f => GSafeArbitrarySum (M1 C c f) where
  gsafeArbitrarySum = pure (1, M1 <$> gsafeArbitrary)

instance GSafeArbitrary f => GSafeArbitrary (M1 S c f) where
  gsafeArbitrary = M1 <$> gsafeArbitrary

instance (GSafeArbitrary f, GSafeArbitrary g) => GSafeArbitrary (f :*: g) where
  gsafeArbitrary = liftA2 (:*:) gsafeArbitrary gsafeArbitrary

instance (SafeArbitrary a) => GSafeArbitrary (K1 i a) where
  gsafeArbitrary = K1 <$> safeArbitrary

instance GSafeArbitrary U1 where gsafeArbitrary = pure U1

instance SafeArbitrary a => SafeArbitrary (Maybe a) where
  safeArbitrary = frequency [(1, pure Nothing), (3, Just <$> safeArbitrary)]

instance SafeArbitrary a => SafeArbitrary [a] where
  safeArbitrary = frequency [(1, pure []), (3, liftA2 (:) safeArbitrary safeArbitrary)]

{- ORMOLU_DISABLE -}

instance SafeArbitrary a => SafeArbitrary (NonEmpty a)
instance (SafeArbitrary a, SafeArbitrary b) => SafeArbitrary (Either a b)

instance (SafeArbitrary a, SafeArbitrary b) => SafeArbitrary (a, b)
instance (SafeArbitrary a, SafeArbitrary b, SafeArbitrary c) => SafeArbitrary (a, b, c)
instance (SafeArbitrary a, SafeArbitrary b, SafeArbitrary c, SafeArbitrary d) => SafeArbitrary (a, b, c, d)
instance (SafeArbitrary a, SafeArbitrary b, SafeArbitrary c, SafeArbitrary d, SafeArbitrary e) => SafeArbitrary (a, b, c, d, e)

instance SafeArbitrary Bool

instance SafeArbitrary Char   where safeArbitrary = arb
instance SafeArbitrary Double where safeArbitrary = arb
instance SafeArbitrary Float  where safeArbitrary = arb

instance SafeArbitrary Integer where safeArbitrary = arb
instance SafeArbitrary Ordering
instance SafeArbitrary ()

instance SafeArbitrary Int            where safeArbitrary = arb
instance SafeArbitrary Foreign.Int8   where safeArbitrary = arb
instance SafeArbitrary Foreign.Int16  where safeArbitrary = arb
instance SafeArbitrary Foreign.Int32  where safeArbitrary = arb
instance SafeArbitrary Foreign.Int64  where safeArbitrary = arb
instance SafeArbitrary Foreign.Word   where safeArbitrary = arb
instance SafeArbitrary Foreign.Word8  where safeArbitrary = arb
instance SafeArbitrary Foreign.Word16 where safeArbitrary = arb
instance SafeArbitrary Foreign.Word32 where safeArbitrary = arb
instance SafeArbitrary Foreign.Word64 where safeArbitrary = arb

instance Integral a      => SafeArbitrary (Data.Ratio.Ratio a) where safeArbitrary = arb
instance SafeArbitrary a => SafeArbitrary (Data.Complex.Complex a)

deriving newtype instance SafeArbitrary a => SafeArbitrary (Control.Applicative.ZipList a)
deriving newtype instance SafeArbitrary a => SafeArbitrary (Control.Monad.Identity a)

deriving newtype instance                    SafeArbitrary Data.Monoid.All
deriving newtype instance                    SafeArbitrary Data.Monoid.Any
deriving newtype instance SafeArbitrary a => SafeArbitrary (Data.Monoid.First a)
deriving newtype instance SafeArbitrary a => SafeArbitrary (Data.Monoid.Last a)
deriving newtype instance SafeArbitrary a => SafeArbitrary (Data.Monoid.Dual a)
deriving newtype instance SafeArbitrary a => SafeArbitrary (Data.Monoid.Sum a)
deriving newtype instance SafeArbitrary a => SafeArbitrary (Data.Monoid.Product a)

instance SafeArbitrary Foreign.C.CChar      where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CSChar     where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CUChar     where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CShort     where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CUShort    where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CInt       where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CUInt      where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CLong      where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CULong     where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CLLong     where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CULLong    where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CFloat     where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CDouble    where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CPtrdiff   where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CSize      where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CWchar     where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CSigAtomic where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CClock     where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CTime      where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CUSeconds  where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CSUSeconds where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CIntPtr    where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CUIntPtr   where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CIntMax    where safeArbitrary = arb
instance SafeArbitrary Foreign.C.CUIntMax   where safeArbitrary = arb

instance SafeArbitrary Test.QuickCheck.Modifiers.PrintableString where safeArbitrary = arb
instance SafeArbitrary Test.QuickCheck.Modifiers.UnicodeString   where safeArbitrary = arb
