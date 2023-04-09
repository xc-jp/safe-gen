import Control.Monad
import Test.Hspec
import Test.QuickCheck as QC
import Test.QuickCheck.SafeGen as Safe

main :: IO ()
main =
  hspec $
    describe "safer-gen-test" $
      it "works" $
        2 + 2 `shouldBe` (4 :: Int)

-- loops = choice [choice [Gen undefined], loops]

-- terminates = choice [terminates, Gen undefined]

-- data Trie a
--   = Leaf a
--   | Branch a (Trie a) (Trie a) (Trie a)
--   deriving (Show, Foldable)

-- instance Arbitrary a => Arbitrary (Trie a) where
--   arbitrary = runSafeGenGrow go
--     where
--       go =
--         Safe.oneof
--           [ con1 Leaf (gen arbitrary),
--             con4 Branch (gen arbitrary) go go go
--           ]

-- data Term a
--   = Var a
--   | Lam (Term (Maybe a))
--   | App (Term a) (Term a)
--   deriving (Show)

-- ball :: Gen (Trie ())
-- ball = runSafeGenGrow go
--   where
--     go =
--       Safe.frequency
--         [ (1, con1 Leaf (gen arbitrary)),
--           (1, con4 Branch (gen arbitrary) (Logic.once go) go go)
--         ]

-- nobal :: Gen (Trie ())
-- nobal = runSafeGenGrow go
--   where
--     go =
--       Safe.frequency
--         [ (1, Leaf <$> gen arbitrary),
--           (1, Branch <$> gen arbitrary <*> go <*> go <*> go)
--         ]

-- tbal :: Trie a -> Maybe (Int, Int, Int)
-- tbal (Leaf _) = Nothing
-- tbal (Branch _ a b c) = Just (length a, length b, length c)

-- instance Arbitrary a => Arbitrary (Term a) where
--   arbitrary = runSafeGenGrow (go arb)
--     where
--       go :: SafeGen a -> SafeGen (Term a)
--       go x =
--         Safe.oneof
--           [ Var <$> x,
--             con2 App (go x) (go x),
--             con1 Lam (go (Safe.oneof [pure Nothing, Just <$> x]))
--           ]

-- sampleBig :: Show a => Gen a -> IO ()
-- sampleBig m = forM_ [1, 5 .. 100] $ \s -> generate (resize s m) >>= print
