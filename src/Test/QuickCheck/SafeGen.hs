{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.QuickCheck.SafeGen
  ( runSafeGenGrow,
    runSafeGen,
    runSafeGenMany,
    runSafeGenAll,
    gen,
    arb,
    liftGen,
    frequency,
    oneof,
    con0,
    con1,
    con2,
    con3,
    con4,
    con5,
    spend,
    SafeGen,
    Budget,
    withCap,
    Balanced,
    balance,
    blift,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic (LogicT, observeAllT, observeManyT)
import Control.Monad.Logic.Class (MonadLogic)
import Control.Monad.State
import Data.Maybe (listToMaybe)
import Data.Sequence (Seq, ViewL (..), (<|))
import qualified Data.Sequence as Seq
import Data.Word (Word64)
import System.Random (genWord64)
import Test.QuickCheck (Arbitrary, arbitrary, getSize, shuffle, sized)
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Random

newtype Budget = Budget Int
  deriving stock (Show)
  deriving newtype (Enum, Ord, Num, Eq, Real, Integral)

newtype SafeGen a = SafeGen (StateT Budget (LogicT Gen) a)
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus, MonadState Budget, MonadLogic)

-- | A convenience wrapper around 'runSafeGen' that, if search completely fails, increases the budget by one, and tries again, until search succeeds.
--
-- The reason this is useful is that in practice, the most common cause of search failure is when a search has a (small) minimum cost.
-- For example, if we imagine our budget to represent a budget for constructors, a value will always consist of at least one constructor, so search fails for the initial budget of 0.
--
-- Of course, this means that if the search uses infinite budget (e.g. @forever (spend 1)@), this will not terminate.
-- Similarly, if your search space has a large minimum cost, it might take a long time to exhaustively search.
runSafeGenGrow :: SafeGen a -> Gen a
runSafeGenGrow (SafeGen m) = sized go
  where
    go size = do
      r <- observeManyT 1 (evalStateT m (Budget size))
      case r of
        a : _ -> pure a
        _ -> go (size + 1)

-- | Run a 'SafeGen' search in 'Gen'.
-- Returns 'Nothing' if search fails.
-- Consider using 'runSafeGenGrow'.
runSafeGen :: SafeGen a -> Gen (Maybe a)
runSafeGen (SafeGen m) = do
  s <- getSize
  listToMaybe <$> observeManyT 1 (evalStateT m (Budget s))

-- | Run a 'SafeGen' search in 'Gen', returning the first n results.
runSafeGenMany :: Int -> SafeGen a -> Gen [a]
runSafeGenMany n (SafeGen m) = do
  s <- getSize
  observeManyT n $ evalStateT m (Budget s)

-- | Run a 'SafeGen' search in 'Gen', returning the all results.
runSafeGenAll :: SafeGen a -> Gen [a]
runSafeGenAll (SafeGen m) = do
  s <- getSize
  observeAllT $ evalStateT m (Budget s)

-- | Subtract an amount from the budget, failing if there is not enough remaining budget.
-- Nothing prevents you from passing a negative value, thereby increasing the remaining budget.
spend :: Int -> SafeGen ()
spend cost = do
  Budget budget <- get
  guard (budget >= cost)
  put (Budget $ budget - cost)

-- | Spends 1, and lifts an action from `Gen` into `SafeGen`.
-- To lift without spending, see 'liftGen'
gen :: Gen a -> SafeGen a
gen m = do
  spend 1
  liftGen m

-- | Short for @'gen' 'arbitrary'@
arb :: Arbitrary a => SafeGen a
arb = gen arbitrary

-- | Lifts an action from `Gen` into `SafeGen`.
-- Consider using 'gen' instead.
liftGen :: Gen a -> SafeGen a
liftGen = SafeGen . lift . lift

-- | Randomly pick one of the branches.
-- If the picked branch fails, it retries the remaining alternatives.
-- If no branch succeeds, this action fails.
oneof :: [SafeGen a] -> SafeGen a
oneof branches = liftGen (shuffle branches) >>= msum

-- | Like 'oneof', but with each branch being assigned a weight.
frequency :: [(Int, SafeGen a)] -> SafeGen a
frequency branches = do
  qc <- liftGen $ MkGen const
  let branches' = [(fromIntegral w, a) | (w, a) <- branches, w >= 1]
  msum (weightedShuffle branches' qc)

weightedShuffle :: [(Word64, a)] -> QCGen -> [a]
weightedShuffle branches = go (sum $ fst <$> branches) (Seq.fromList branches)
  where
    lookupIx :: Word64 -> Seq (Word64, a) -> (a, Word64, Seq (Word64, a))
    lookupIx ix = go' 0 mempty
      where
        go' run prev s = case Seq.viewl s of
          EmptyL -> error "Internal weightedShuffle error!"
          h@(w, a) :< as ->
            let run' = run + w
             in if ix < run'
                  then (a, w, prev <> as)
                  else go' run' (h <| prev) as
    go :: Word64 -> Seq (Word64, a) -> QCGen -> [a]
    go _ Seq.Empty _ = []
    go remSum brs qc =
      let (w, qc') = genWord64 qc
          (a, wa, brs') = lookupIx (mod w remSum) brs
       in a : go (remSum - wa) brs' qc'

-- | Put a cap on how much budget the inner action is allowed to use.
withCap :: Budget -> SafeGen a -> SafeGen a
withCap cap m = do
  remain <- get
  if remain <= cap
    then m
    else do
      let reserve = remain - cap
      put cap
      a <- m
      modify (+ reserve)
      pure a

-- | Run a 'Balanced' search.
-- 'Balanced' approximately evenly divides the budget between each of its inner actions.
-- You lift actions into 'Balanced' using 'blift', combine them through the Applicative instance, and run them using 'balanced'.
--
-- Normally, you would use the 'con2', 'con3', etc. functions that are built on top of this, but if those don't fit your use case for whatever reason, this gives you access to the underlying machinery.
--
-- Here's an example:
--
-- > con3 :: (a -> b -> c -> d) -> SafeGen a -> SafeGen b -> SafeGen c -> SafeGen d
-- > con3 f a b c = do
-- >   'spend' 1
-- >   'balance' $ f <$> 'blift' a <*> 'blift' b <*> 'blift' c
--
-- The point of balancing is not just to give constructors a roughly equal number of children in every branch.
-- The primary reason is that blindly chaining actions in a backtracking search (as @liftM*@, @<*>@ etc. do) is an easy way to blow up the search space.
-- Balancing mitigates this by front-loading failure; the first actions are run under the tightest budget constraints, and every subsequent actions has at least that much budget left (more details in the next paragraph).
-- In cases where this is not enough, the 'SafeGen' has a 'MonadLogic' instance provides more tools for curtailing the search space.
--
-- 'Balance' works by keeping track of how many inner actions are being run.
-- When running 'balance' on a 'Balance' containing N actions, we first run the first action, allowing it to only use 1/N of the budget.
-- Then, we run the second action, with 1/(N-1) of the remaining budget, and so on.
-- It's hard to actually observe, but the above means that 'Balance' is technically not a lawful 'Applicative' (proof left as an exercise for the reader).
balance :: Balanced a -> SafeGen a
balance (Pure m) = m
balance (Balanced rl bl rr br) = do
  totalBudget <- get
  let leftBranchBudget = floor $ fromIntegral totalBudget * rl / (rl + rr)
      withHeld = totalBudget - leftBranchBudget
  put leftBranchBudget
  f <- balance bl
  modify (+ withHeld)
  a <- balance br
  pure (f a)

-- | Lift a 'SafeGen' action into 'Balanced'
-- See 'balance' for more info.
blift :: SafeGen a -> Balanced a
blift = Pure

-- | Spend 1, and return a value
con0 :: a -> SafeGen a
con0 a = a <$ spend 1

-- | Spend 1, run an action, and apply a function to the result.
con1 :: (a -> b) -> SafeGen a -> SafeGen b
con1 f a = spend 1 >> f <$> a

-- | Spend 1, run two actions, and combine their results with the given function.
-- This approximately evenly divides the budget between the two actions.
-- For more details, and a discussion of how this is different from 'liftA2', see 'balance'.
con2 :: (a -> b -> c) -> SafeGen a -> SafeGen b -> SafeGen c
con2 f a b = spend 1 >> balance (liftA2 f (blift a) (blift b))

-- | Spend 1, run three actions, and combine their results with the given function.
-- This approximately evenly divides the budget between the three actions.
-- For more details, and a discussion of how this is different from 'liftA3', see 'balance'.
con3 :: (a -> b -> c -> d) -> SafeGen a -> SafeGen b -> SafeGen c -> SafeGen d
con3 f a b c = spend 1 >> balance (f <$> blift a <*> blift b <*> blift c)

-- | Spend 1, run four actions, and combine their results with the given function.
-- This approximately evenly divides the budget between the four actions.
-- For more details, and a discussion of how this is different from 'liftA4', see 'balance'.
con4 :: (a -> b -> c -> d -> e) -> SafeGen a -> SafeGen b -> SafeGen c -> SafeGen d -> SafeGen e
con4 f a b c d = spend 1 >> balance (f <$> blift a <*> blift b <*> blift c <*> blift d)

-- | Spend 1, run five actions, and combine their results with the given function.
-- This approximately evenly divides the budget between the five actions.
-- For more details, and a discussion of how this is different from 'liftA5', see 'balance'.
con5 :: (a -> b -> c -> d -> e -> f) -> SafeGen a -> SafeGen b -> SafeGen c -> SafeGen d -> SafeGen e -> SafeGen f
con5 f a b c d e = spend 1 >> balance (f <$> blift a <*> blift b <*> blift c <*> blift d <*> blift e)

data Balanced a
  = forall p. Balanced Double (Balanced (p -> a)) Double (Balanced p)
  | Pure (SafeGen a)

deriving instance Functor Balanced

instance Applicative Balanced where
  pure = Pure . pure
  l <*> r = Balanced (bias l) l (bias r) r
    where
      bias :: Balanced a -> Double
      bias (Balanced bl _ br _) = bl + br
      bias (Pure _) = 1
