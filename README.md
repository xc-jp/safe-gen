# safe-gen

[![safe-gen on hackage](https://img.shields.io/hackage/v/safe-gen)](http://hackage.haskell.org/package/safe-gen)
[![safe-gen on Stackage Nightly](https://stackage.org/package/safe-gen/badge/nightly)](https://stackage.org/nightly/package/safe-gen)

Writing `Arbitrary` instances is for recursive data is annoying, since it's easy to accidentally write non-terminating instances.
A common pattern is to use `Gen`'s implicit size parameter to guide recursion, but this is tedious and error-prone.
`safe-gen` automates this process, and makes it so that the naive implementation always terminates.
It also adds a mechanism for deriving `Arbitrary` instances.

## Example

Here's an example of an `Arbitrary` instance that does not terminate:

```haskell
data Trie a
  = Leaf a
  | Branch (Trie a) (Trie a) (Trie a)

instance Arbitrary a => Arbitrary (Trie a) where
  arbitrary = oneof
    [ Leaf <$> arbitrary
    , Branch <$> arbitrary <*> arbitrary <*> arbitrary
    ]
```

And here's how to write it manually using `safe-gen`:

```haskell
instance Arbitrary a => Arbitrary (Trie a) where
  arbitrary = Safe.runSafeGen go
    where go = Safe.oneof
                 [ Leaf <$> Safe.gen arbitrary,
                   Branch <$> go <*> go <*> go
                 ]
```

Or, derive it automatically:

```haskell
data Trie a
  = Leaf a
  | Branch (Trie a) (Trie a) (Trie a)
  deriving stock Generic
  deriving anyclass SafeArbitrary
  deriving Arbitrary via FromSafeArbitrary (Trie a)
```

## Usage

`safe-gen` has a very small API. The two code samples show it off almost all there is to it.

When manually writing instances, use the `Test.QuickCheck.SafeGen` module.
`runSafeGen :: SafeGen a -> Gen a` and `gen :: Gen a -> SafeGen a` convert back and forth between `SafeGen` and `Gen`.
Within `SafeGen`, compose product types using the `Applicative` interface, and sum types using `frequency` or `oneof`.
`arb` is a convenient synonym for `gen arbitrary`.

For the generics, use the `Test.QuickCheck.SafeGen.Generic` module.
It exposes the `SafeArbitrary` type class, which can be derived automatically, and the `FromSafeArbitrary` wrapper for deriving an `Arbitrary` from `SafeArbitrary`.

## Design

As mentioned in the intro, a common pattern is to use `Gen`'s implicit size parameter to guide recursion, but this is tedious and error-prone.
`safe-gen` automates this by
  1. automatically dividing the size parameter between the different branches of a product type, and
  2. only going down sufficiently shallow branches of a sum type.

The guarantee this gives you is that _if there is a terminating code path_, `safe-gen` finds it without you needing to worry about manually controlling recursion.
That means that (despite the name) `safe-gen` does not make it completely impossible to write bottom `Gen` values.
Nothing prevents you from writing something like `runSafeGen (let go = go in go)`, although in some cases, such as `runSafeGen (let f = oneof [f] in f)`, `safe-gen` will be able to catch the infinite recursion and throw an exception.
If you have a lazy infinite generator, `safe-gen` (like vanilla `Gen`, and unlike `generic-arbitrary`) gives you a lazy infinite value.

### Implementation details

A `SafeGen` value is a potentially infinite n-ary tree where branches are either product types or sum types, and leaves are ordinary `Gen` values.

For every node, we define a _shallowness_ value, or minimum depth, which is the smallest number of branches to traverse to find a leaf.
Since trees are potentially infinite, we take care to calculate this value lazily as a Peano numeral.
For sum types, the shallowness is one plus the shallowness of its shallowest branch, for product types it is one plus the shallowness of its deepest branch, and for leaves it is zero.

When we "run" this tree, i.e. go from `SafeGen` to `Gen`, we get `Gen`'s size parameter.
For product branches, we divide the size between the number of branches and recurse.
For sum branches, we choose only branches whose shallowness is at most the current size, or if none exist, the shallowest branch.

### Comparison with other packages

`safe-gen` exists in a similar space as [`generic-arbitrary`](https://github.com/typeable/generic-arbitrary).
Both make writing `Arbitrary` instances less error-prone.
The philosophical difference is that `generic-arbitrary` does most of its heavy lifting upfront and at the type level, whereas `safe-gen` works entirely at the value level.

The benefit of `generic-arbitrary` is that it can provide guarantees at compile time, and only requires a single derived instance.
In most cases, `generic-arbitrary` will be able to derive useful instances, and there is no reason to use `safe-gen`.
Unfortunately, there are also many cases where it can't derive useful instances, and it won't always warn you.
In those cases, you might be interested in using `safe-gen` instead.
Examples include when you have mutual recursion, tricky fixpoints, or invariants to maintain.

There is also [less-arbitrary](https://github.com/mgajda/less-arbitrary) which is quite a bit more complex than either `safe-gen` or `generic-arbitrary`, and has a larger API and dependency footprint.
It throws exceptions and retries if it can't find a terminating instance, whereas `safe-gen` tries to see if a path terminates upfront.
