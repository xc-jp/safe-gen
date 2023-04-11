import Control.Applicative
import Control.Monad (forM_)
import Test.Hspec
import Test.QuickCheck as QC
import Test.QuickCheck.SafeGen as Safe

main :: IO ()
main =
  hspec $
    describe "safe-gen-test" $ do
      it "Trie terminates" $
        generatorTerminates . runSafeGen $
          let go = Safe.oneof [pure (Leaf ()), liftA3 Branch go go go]
           in go
      it "Unbalanced Trie terminates" $
        generatorTerminates . runSafeGen $
          let go =
                Safe.frequency
                  [ (1, pure (Leaf ())),
                    (10, liftA3 Branch go go go)
                  ]
           in go
      it "gen . runSageGen terminates" $
        generatorTerminates . runSafeGen $
          let go =
                Safe.oneof
                  [ pure (Leaf ()),
                    gen (runSafeGen go),
                    liftA3 Branch go go go
                  ]
           in go
      it "Ignores a looping branch" $
        generatorTerminates . runSafeGen $
          let loop = Safe.oneof [loop]
              go =
                Safe.oneof
                  [ pure (Leaf ()),
                    loop
                  ]
           in go
      it "Throws on infinite generator" $
        let go = Safe.oneof [go] :: SafeGen [()]
         in shouldThrow (generatorTerminates $ runSafeGen go) anyException
      it "Generates terms" $
        generatorTerminates . runSafeGen $
          let go :: SafeGen a -> SafeGen (Term a)
              go x =
                Safe.oneof
                  [ Var <$> x,
                    Lam <$> go (Safe.frequency [(3, Just <$> x), (1, pure Nothing)]),
                    liftA2 App (go x) (go x)
                  ]
           in go (pure ())

generatorTerminates :: Gen a -> Expectation
generatorTerminates g = flip shouldReturn () $ do
  as <- sample' g
  forM_ as $ \a -> seq a (pure ())

data Trie a = Leaf a | Branch (Trie a) (Trie a) (Trie a)

data Term a
  = Var a
  | Lam (Term (Maybe a))
  | App (Term a) (Term a)
