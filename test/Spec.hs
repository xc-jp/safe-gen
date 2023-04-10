import Control.Applicative
import Control.Monad (forM_)
import Test.Hspec
import Test.QuickCheck as QC
import Test.QuickCheck.SafeGen as Safe

main :: IO ()
main =
  hspec $
    describe "safe-gen-test" $ do
      it "throws on infinite generator" $
        let go :: SafeGen [()]
            go = Safe.oneof [go]
         in shouldThrow (generatorTerminates go) anyException
      it "Trie terminates" $
        generatorTerminates $
          let go = Safe.oneof [pure (Leaf ()), liftA3 Branch go go go]
           in go
      it "Unbalanced Trie terminates" $
        generatorTerminates $
          let go =
                Safe.frequency
                  [ (1, pure (Leaf ())),
                    (10, liftA3 Branch go go go)
                  ]
           in go
      it "gen . runSageGen terminates" $
        generatorTerminates $
          let go =
                Safe.oneof
                  [ pure (Leaf ()),
                    gen (runSafeGen go),
                    liftA3 Branch go go go
                  ]
           in go

generatorTerminates :: SafeGen a -> Expectation
generatorTerminates sg = flip shouldReturn () $ do
  as <- sample' (runSafeGen sg)
  forM_ as $ \a -> seq a (pure ())

data Trie a = Leaf a | Branch (Trie a) (Trie a) (Trie a)
