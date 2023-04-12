# safe-gen

[![safe-gen on hackage](https://img.shields.io/hackage/v/safe-gen)](http://hackage.haskell.org/package/safe-gen)
[![safe-gen on Stackage Nightly](https://stackage.org/package/safe-gen/badge/nightly)](https://stackage.org/nightly/package/safe-gen)

Writing `Arbitrary` instances is for recursive data is annoying, since it's easy to accidentally write non-terminating instances.
A common pattern is to use `Gen`'s implicit size parameter to guide recursion, but this is tedious and error-prone.
`safe-gen` automates this process, and makes it so that the naive implementation always terminates.

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
    , Branch <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]
```

And here's how to write it safely using `safe-gen`.

```haskell
instance Arbitrary a => Arbitrary (Trie a) where
  arbitrary = runSafeGen go
    where go = Safe.oneof
                 [ Leaf <$> Safe.gen arbitrary,
                   Branch <$> Safe.gen arbitrary <*> go <*> go <*> go
                 ]
```

## Usage

`safe-gen` has a very small API.
`runSafeGen :: SafeGen a -> Gen a` and `gen :: Gen a -> SafeGen a` convert back and forth between `SafeGen` and `Gen`.
Within `SafeGen`, compose product types using the `Applicative` interface, and sum types using `frequency` or `oneof`.
`arb` is a convenient synonym for `gen arbitrary`.

## Design and implementation

As mentioned in the intro, a common pattern is to use `Gen`'s implicit size parameter to guide recursion, but this is tedious and error-prone.
`safe-gen` automates this by automatically dividing the size parameter between the different branches of a product type, and only considering sufficiently shallow branches of a sum type.

`safe-gen` exists in a similar space as [generic-arbitrary](https://github.com/typeable/generic-arbitrary).
Both make writing `Arbitrary` instances less error-prone, in the case of `generic-arbitrary` by not having to write them at all.
If automatically deriving `Generic` instances works for your use case, there is probably no reason to use `safe-gen`.
However, there are cases where `generic-arbitrary` fails to derive useful terminating instances, and in those cases `safe-gen` might help.

There is also [less-arbitrary](https://github.com/mgajda/less-arbitrary) which is quite a bit more complex than either `safe-gen` or `generic-arbitrary`, and has a larger API and dependency footprint.
It throws exceptions and retries if it can't find a terminating instance, whereas `safe-gen` tries to take paths that it knows terminate.

Despite the name, `safe-gen` does not make it completely impossible to write non-terminating `Gen` values.
Nothing prevents you from writing something like `runSafeGen (let go = go in go)`, and it won't magically give you a lazy generator for infinite data (think `data Stream a = Stream a (Stream a)`).
Instead, if there is a terminating code path, `safe-gen` finds it without you needing to worry about manually controlling recursion.
Also, in some cases, such as `runSafeGen (let f = oneof [f] in f)`, `safe-gen` will be able to catch the infinite recursion upfront, and throw an exception.

### Implementation details

A `SafeGen` value is a potentially infinite n-ary tree where branches are either product types or sum types, and leaves are `Gen` values.

For every node, we define a _shallowness_ value, or minimum depth, which is the smallest number of branches to traverse to find a leaf.
Since trees are potentially infinite, we take care to calculate this value lazily as a Peano numeral.
For sum types, the shallowness is one plus the shallowness of its shallowest branch, for product types it is one plus the shallowness of its deepest branch, and for leaves it is zero.

When we "run" this tree, i.e. go from `SafeGen` to `Gen`, we get `Gen`'s size parameter.
For product branches, we divide the size between the number of branches and recurse.
For sum branches, we choose only branches whose shallowness is at most the current size, or if none exist, the shallowest branch.
