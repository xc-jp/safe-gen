# safe-gen

[![safe-gen on hackage](https://img.shields.io/hackage/v/safe-gen)](http://hackage.haskell.org/package/safe-gen)
[![safe-gen on Stackage Nightly](https://stackage.org/package/safe-gen/badge/nightly)](https://stackage.org/nightly/package/safe-gen)

A common annoyance when writing `Arbitrary` instances is accidentally writing non-terminating generators for recursive data.
`safe-gen` allows you to write almost the exact same code, but have them be guaranteed to terminate.
It does this by automatically dividing `Gen`'s implicit size parameter between the different branches of a product type, and then only selecting sufficiently shallow branches of a sum type.

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

## Design and goals

A common pattern when writing recursive `Arbitrary` instances is to manually decrease the size parameter when recursing.
This is annoying and tricky to get right.
`safe-gen` automates this.

`safe-gen` exists in a similar space as [generic-arbitrary](https://github.com/typeable/generic-arbitrary).
Both make writing `Arbitrary` instances less error-prone, in the case of `generic-arbitrary` by not having to write them at all.
If that works for your use case, there is no reason to use `safe-gen`.
If it doesn't work for you, you might be interested in `safe-gen` instead.
For example, you might have invariants to maintain, are dealing with tricky fixpoints, or simply don't have a `Generic` instance.

Despite the name, and unlike `generic-arbitrary`, `safe-gen` does not make it completely impossible to write non-terminating `Gen` values.
Nothing prevents you from writing something like `runSafeGen (let go = go in go)`, and it won't magically give you a lazy generator for infinite data (think `data Stream a = Stream a (Stream a)`).
Instead, if there is a terminating code path, `safe-gen` finds it without you needing to worry about manually controlling recursion.
Also, in some cases, such as `runSafeGen (let f = oneof [f] in f)`, `safe-gen` will be able to catch the infinite recursion upfront, and throw an exception.
