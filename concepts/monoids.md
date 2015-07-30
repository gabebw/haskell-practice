A Monoid is useful when an operation has to combine results.

* Every monoid has an operation <> :: m -> m -> m
* Every monoid has a particular value `mempty` such that
  `x <> mempty == x` and `mempty <> x == x`

Specifically:

```haskell
class Monoid m where
  mempty  :: m
  -- There's also `<>`, which is infix `mappend`
  -- Note that mappend is associative: `x <> y == y <> x`
  mappend :: m -> m -> m

  -- `mconcat` can be omitted from Monoid instances because it has
  -- a default implementation in Haskell
  mconcat :: [m] -> m
  mconcat []     = mempty
  mconcat (x:xs) = x `mappend` mconcat xs
```

Some examples with lists:

```haskell
mempty :: [a] == []
mappend [1] [2] == [1, 2]
[1] <> [2] <> [3] == [1, 2, 3]
mconcat [[3], [4]] == [3, 4]
```
