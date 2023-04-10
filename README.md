# safe-gen

[![safe-gen on hackage](https://img.shields.io/hackage/v/safe-gen)](http://hackage.haskell.org/package/safe-gen)
[![safe-gen on Stackage Nightly](https://stackage.org/package/safe-gen/badge/nightly)](https://stackage.org/nightly/package/safe-gen)

A common annoyance when writing `Arbitrary` instances is accidentally writing diverging instances for recursive data.
This package aims to make it easier to write instances that are guaranteed to terminate.
It does this by reinterpreting `Gen`'s implicit size parameter as a budget, and performing a backtracking search for values that stay within this budget.

## Example

Here's an example of an `Arbitrary` that diverges:

```haskell
data Trie a
  = Leaf a
  | Branch a (Trie a) (Trie a) (Trie a)
  deriving (Show, Foldable)

instance Arbitrary a => Arbitrary (Trie a) where
  arbitrary = oneof
    [ Leaf <$> arbitrary
    , liftM4 Branch arbitrary arbitrary arbitrary arbitrary
    ]
```

And here's how to write it safely using `safe-gen`.

```haskell
instance Arbitrary a => Arbitrary (Trie a) where
  arbitrary = runSafeGenGrow go
    where go :: SafeGen (Trie a)
          go = Safe.oneof
                 [ con1 Leaf (liftGen arbitrary),
                   con4 Branch (liftGen arbitrary) go go go
                 ]
```

## Design and goals

`safe-gen`'s goal is to help write useful terminating instances, and to get as little in the way as possible.
It exists in a similar space as [generic-arbitrary](https://github.com/typeable/generic-arbitrary), but is aimed specifically at scenarios where deriving instances does not work.
Examples include when you have invariants to maintain, are dealing with tricky fixpoints, or simply don't have a `Generic` instance.

Unlike `generic-arbitrary`, it is specifically not a goal to _guarantee_ termination.
Because `SafeGen` is based on a backtracking search instead of `Gen`'s Monte Carlo search, there are a number of footguns in terms of blowing up the search space that we don't prevent you from accidentally triggering.
Caveat emptor.

## Usage

Conceptually, the main two points of interest are the `SafeGen` monad, and `spend :: Int -> SafeGen ()`.
If we try to spend more than our budget allows, this branch fails, and we backtrack.
Branching is done using `SafeGen`'s `Alternative`/`MonadPlus` instances.

Here's a simple example that

```haskell
trueIsExpensive :: SafeGen Bool
trueIsExpensive = (True <$ spend 10) <|> pure False
```

Generated with [template-haskell](https://github.com/jonascarpay/template-haskell)
