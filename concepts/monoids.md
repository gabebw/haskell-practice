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

## Monoids under Bool

```haskell
-- &&
> mconcat [All True, All False]
All {getAll = False}
> mconcat [All True, All True]
All {getAll = True}

-- ||
> mconcat [Any False, Any False]
Any {getAny = False}
> mconcat [Any True, Any False]
Any {getAny = True}
```

## Monoids under numbers

```haskell
-- Add numbers
> mconcat [Sum 1, Sum 2]
Sum {getSum = 3}

-- Multiply numbers
> mconcat [Product 1, Product 2]
Product {getProduct = 2}
```

## Monoids under Maybe

```haskell
-- First gives the first non-Nothing value
> mconcat [First Nothing, First (Just 3), First (Just 4)]
First {getFirst = Just 3}

-- Last gives the last non-Nothing value
> mconcat [Last Nothing, Last (Just 3), Last (Just 4)]
Last {getLast = Just 4}
```
