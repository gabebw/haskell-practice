## `::`

```
ghci> :t 'a'
'a' :: Char
```

* `::` means "type of"
* `removeNonUppercase` has a type of [Char] -> [Char], meaning that it maps from a string to a string.

```haskell
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
```

### Common Types

## Int

Int stands for integer. It's used for whole numbers. It's bounded, so it has a
minimum/maximum value.

## Integer

Unbounded version of Int, for really big numbers.

## Float

Float is a real floating point with single precision.

## Bool

Bool is a boolean type. It can have only two values: `True` and `False`.

## Char

Char represents a character. It's denoted by single quotes. A list of characters
is a String.

## String

String is 100% exactly the same as `[Char]`.


Type Variables
==============

```
ghci> :t head
head :: [a] -> a
```

* `a` is a *type variable*: `a` can be of any type
* functions that have type variables are called polymorphic functions
* The type declaration of head states that it takes a list of any type and returns one element of that type
* Although type variables can have names longer than one character, we usually give them names of a, b, c, d, etc

## Typeclasses

* A typeclass is a sort of interface that defines some behavior (like Java
  interfaces)

    ghci> :t (==)
    (==) :: (Eq a) => a -> a -> Bool

* Everything before `=>` is called a class constraint. Here, it says
  "== works on any a, where a is part of the Eq typeclass"

## Eq

Eq is used for types that support equality testing. The functions its members
implement are `==` and `/=.`

## Ord

* Ord is for types that have an ordering.
* Ord covers all the standard comparing functions such as `>`, `<`, `>=` and
 `<=`.
* The compare function takes two Ord members of the same type and returns an
  ordering. Ordering is a type that can be GT, LT or EQ, meaning greater than,
 lesser than and equal, respectively.

    ghci> "Abrakadabra" `compare` "Zebra"
    LT
    ghci> 5 `compare` 3
    GT

## Show

Members of Show can be presented as strings.
Use `show` to transform a value into a String representation.

```
ghci> show 3
"3"
ghci> show 5.334
"5.334"
ghci> show True
"True"
```

## Read

Read is like the opposite of Show.
The `read` function takes a string and returns a type which is a member of Read.

```
ghci> read "8.2" + 3.8
12.0
```

If you're not using the value in an expression, Haskell can't figure out what
you want, and you need to give it a type: `read 4 :: Int`.

## Enum

Enum members are sequentially ordered types — they can be enumerated.
The main advantage of the Enum typeclass is that we can use its types in list ranges.
They also have defined successors and predecesors, which you can get with the
`succ` and `pred` functions.

If you make a Card type (like in a deck of cards), you might make it enumerable.

```
ghci> ['a'..'e']
"abcde"
```


Num
---

Numeric typeclass. To join Num, a type must already be friends with Show and Eq.
Includes Integer, Float, and Double.

## Pattern matching

```haskell
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"
```
