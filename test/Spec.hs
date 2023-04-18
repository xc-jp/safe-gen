{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad (forM_)
import GHC.Generics (Generic)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck as QC
import Test.QuickCheck.SafeGen as Safe

main :: IO ()
main =
  hspec $
    describe "safe-gen-test" $ do
      it "trie terminates" $
        generatorTerminates . runSafeGen $
          let go = Safe.oneof [pure (Leaf ()), liftA3 Branch go go go]
           in go
      it "unbalanced Trie terminates" $
        generatorTerminates . runSafeGen $
          let go =
                Safe.frequency
                  [ (1, pure (Leaf ())),
                    (10, liftA3 Branch go go go)
                  ]
           in go
      it "gen . runSafeGen terminates" $
        generatorTerminates . runSafeGen $
          let go =
                Safe.oneof
                  [ pure (Leaf ()),
                    gen (runSafeGen go),
                    liftA3 Branch go go go
                  ]
           in go
      it "ignores a looping branch" $
        generatorTerminates . runSafeGen $
          let loop = Safe.oneof [loop]
              go =
                Safe.oneof
                  [ pure (Leaf ()),
                    loop
                  ]
           in go
      it "throws on infinite generator" $
        let go = Safe.oneof [go] :: SafeGen [()]
         in shouldThrow (generatorTerminates $ runSafeGen go) anyException
      it "prefers shallower branches at low sizes" $
        generatorTerminates . resize 0 . runSafeGen $
          Safe.oneof [pure (), liftA2 undefined (gen undefined) (gen undefined)]
      it "can generate tries of the maximum possible size" $ do
        let go = Safe.frequency [(1, pure (Leaf ())), (1000, liftA3 Branch go go go)]
        ls <- sample' $ resize 81 $ length <$> runSafeGen go
        maximum ls `shouldBe` 81
      prop "arbitrary generators terminate" $ \(Blind (sg :: SafeGen Int)) ->
        generatorTerminates $ runSafeGenNoCheck sg
      it "generates terms" $
        generatorTerminates . runSafeGen $
          let go :: SafeGen a -> SafeGen (Term a)
              go x =
                Safe.oneof
                  [ Var <$> x,
                    Lam <$> go (Safe.frequency [(3, Just <$> x), (1, pure Nothing)]),
                    liftA2 App (go x) (go x)
                  ]
           in go (pure ())
      describe "generics" $ do
        prop "derived Trie generator terminates" $ generatorTerminates (arbitrary :: Gen (Trie Int))
        prop "derived Term generator terminates" $ generatorTerminates (arbitrary :: Gen (Term Int))
        prop "runSafeGen works with lazy infinite data" $ \(Blind (a :: Stream Int)) -> not (null a)

generatorTerminates :: Gen a -> Expectation
generatorTerminates g = flip shouldReturn () $ do
  as <- sample' g
  forM_ as $ \a -> seq a (pure ())

data Trie a = Leaf a | Branch (Trie a) (Trie a) (Trie a)
  deriving stock (Foldable, Generic)
  deriving anyclass (SafeArbitrary)
  deriving (Arbitrary) via FromSafeArbitrary (Trie a)

data Term a
  = Var a
  | Lam (Term (Maybe a))
  | App (Term a) (Term a)
  deriving stock (Foldable, Generic)
  deriving anyclass (SafeArbitrary)
  deriving (Arbitrary) via FromSafeArbitrary (Term a)

data Stream a = Stream a (Stream a)
  deriving stock (Foldable, Generic)
  deriving anyclass (SafeArbitrary)

instance SafeArbitrary a => Arbitrary (Stream a) where arbitrary = runSafeGenNoCheck safeArbitrary
