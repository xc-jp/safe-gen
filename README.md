# safe-gen

[![safe-gen on hackage](https://img.shields.io/hackage/v/safe-gen)](http://hackage.haskell.org/package/safe-gen)
[![safe-gen on Stackage Nightly](https://stackage.org/package/safe-gen/badge/nightly)](https://stackage.org/nightly/package/safe-gen)

Writing `Arbitrary` instances is for recursive data is annoying, since it's easy to accidentally write non-terminating generators.
A common pattern is to use `Gen`'s implicit size parameter to guide recursion, but this is tedious and error-prone.
`safe-gen` automates this process, making recursive instances painless.
`safe-gen` can be used both to manually write instances or to automatically derive them.

## Example

Here's an example of an `Arbitrary` instance that runs in unbounded time:

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
  arbitrary = runSafeGen go
    where go = Safe.oneof
                 [ Leaf <$> gen arbitrary,
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

`safe-gen` has a very small API. The two code samples above show it off almost all there is to it.

To manually build `SafeGen`s, use `gen :: Gen a -> SafeGen a` to construct a single value, compose product types using the `Applicative` interface, and sum types using `frequency` or `oneof`.
To run, use `runSafeGen :: SafeGen a -> Gen a`.
`arb` is a convenient synonym for `gen arbitrary`.

To derive `Arbitrary` instances, you need to first derive a `SafeArbitrary` instance, typically as an empty instance declaration.
The `FromSafeArbitrary` wrapper then provides a convenient way to derive an `Arbitrary` instance from the `SafeArbitrary` instance.

## Design

As mentioned in the intro, a common pattern is to use `Gen`'s implicit size parameter to guide recursion, but this is tedious and error-prone.
`safe-gen` automates this by
  1. automatically dividing the size parameter between the branches of a product type, and
  2. only going down sufficiently shallow branches of a sum type.

This guarantees that **if your generator _can_ terminate in finite time, `safe-gen` makes sure that it _does_.**

The inverse is not true; (despite the name) `safe-gen` does not make it impossible to write bottom/unbounded/diverging `Gen` values.
For example, nothing prevents you from writing something like `runSafeGen (let go = go in go)`, which will cause an infinite loop.
In some cases, such as `runSafeGen (let f = oneof [f] in f)`, `safe-gen` will be able to catch the infinite recursion and throw an exception, and in some cases it will give you a lazy infinite generator, but in general, (and unlike `generic-arbitrary`, about which more below), these issues are left undefined and outside the scope of the library.

### Implementation details

A `SafeGen` value is a lazy, potentially infinite n-ary tree, whose branches are product or sum types, and leaves are `Gen` values.

For every node in the tree, we define a _shallowness_ value, or minimum depth, which is the smallest number of child nodes to traverse to find a leaf.
More precisely, for sum types the shallowness is one plus the shallowness of its shallowest child, for product types it is one plus the shallowness of its least shallow child, and for leaves it is zero.
Since trees are potentially infinite, we take care to calculate this value lazily as a [Peano number](https://wiki.haskell.org/Peano_numbers).

When we "run" this tree, i.e. go from `SafeGen` to `Gen`, we start by asking for `Gen`'s size parameter.
Then, with this size parameter in hand, we traverse the tree as follows:
For sum branches, we only consider children whose shallowness is at most the size parameter, or if none exist, the shallowest branch(es).
For product branches, we divide the size between the number of children, and recurse.

### Comparison with other packages

`safe-gen` exists in a similar space as [`generic-arbitrary`](https://github.com/typeable/generic-arbitrary).
Both make writing `Arbitrary` instances less error-prone.
The philosophical difference is that `generic-arbitrary` does most of its heavy lifting upfront and at the type level, whereas `safe-gen` works entirely at the value level.

Practically, the two benefits of `generic-arbitrary` are that it can provide pretty good guarantees at compile time, and that it only requires a single derived instance.
Its main drawback is that there are cases where it _can't_ derive (useful) instances, and it won't _always_ be able to warn you about that.
Examples include when you have mutual recursion, tricky fixpoints, or invariants to maintain.
`safe-gen` won't be able to preclude certain known-bad generators the way `generic-arbitrary` can, but on the flip side, it will allow certain valid generators that `generic-arbitrary` doesn't, and it makes it easy to dive under the hood to tweak instances where necessary.
The choice is yours, and the buy-in for either is generally low enough that you can't go wrong either way.

There is also [`less-arbitrary`](https://github.com/mgajda/less-arbitrary).
It attempts to address some of `generic-arbitrary`'s shortcomings by adding a heuristic based retry for generators that seem to loop and, like `safe-gen`, provides a way to manually tweak generators.
My main criticism is that, compared to `safe-gen`, its API is complicated, and it's still pretty easy to shoot yourself in the foot and write an unbounded generator.
