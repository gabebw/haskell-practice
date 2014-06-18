

    ghci> :t 'a'
    'a' :: Char

* `::` means "type of"
* removeNonUppercase has a type of [Char] -> [Char], meaning that it maps from a string to a string.

```
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
```

### Common Types

Int
---

Int stands for integer. It's used for whole numbers. 7 can be an Int but 7.2
cannot. Int is bounded, which means that it has a minimum and a maximum value.
Usually on 32-bit machines the maximum possible Int is 2147483647 and the
minimum is -2147483648.

Integer
-------

Integer stands for, er ... also integer. The main difference is that it's not
bounded so it can be used to represent really really big numbers. I mean like
really big. Int, however, is more efficient.


Float
-----

Float is a real floating point with single precision.

Double
------

Double is a real floating point with double the precision!



Bool
----

Bool is a boolean type. It can have only two values: True and False.

Char
----

Char represents a character. It's denoted by single quotes. A list of characters
is a string.


Type Variables
==============

  ghci> :t head
  head :: [a] -> a

* "a" is a *type variable* - "a" can be of any type
* functions that have type variables are called polymorphic functions.
* The type declaration of head states that it takes a list of any type and returns one element of that type.
* Although type variables can have names longer than one character, we usually give them names of a, b, c, d, etc

Typeclasses
===========

* A typeclass is a sort of interface that defines some behavior (like Java
  interfaces)

    ghci> :t (==)
    (==) :: (Eq a) => a -> a -> Bool

* Everything before the => symbol is called a class constraint. Here, it says
  "== works on any a, where a is part of the Eq typeclass"

Eq
--
Eq is used for types that support equality testing. The functions its members implement are `==` and `/=.`


Ord
---

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

Show
----

Members of Show can be presented as strings. All types covered so far except for
functions are a part of Show. The most used function that deals with the Show
typeclass is `show`. It takes a value whose type is a member of Show and presents
it to us as a string.

    ghci> show 3
    "3"
    ghci> show 5.334
    "5.334"
    ghci> show True
    "True"

Read
----

Read is sort of the opposite typeclass of Show. The `read` function takes a string
and returns a type which is a member of Read.


    ghci> read "8.2" + 3.8
    12.0

BUT! If you just do `read "4"`, you need to provide the type because Haskell
can't infer it like it can when you actually use the result of `show` for
something.

    ghci> read "4" :: Int
    4

Enum
----

Enum members are sequentially ordered types — they can be enumerated. The main
advantage of the Enum typeclass is that we can use its types in list ranges.
They also have defined successors and predecesors, which you can get with the
`succ` and `pred` functions. Types in this class: (), Bool, Char, Ordering, Int,
Integer, Float and Double.

    ghci> ['a'..'e']
    "abcde"


Num
---

Numeric typeclass. To join Num, a type must already be friends with Show and Eq.
Includes Integer, Float, and Double.

Integral
--------

Integral is a numeric typeclass. Num includes all numbers, including real
numbers and integral numbers, Integral includes only integral (whole) numbers.
In this typeclass are Int and Integer.

Floating
--------

Floating includes only floating point numbers, so Float and Double.


fromIntegral
------------

A very useful function for dealing with numbers is `fromIntegral`. It has a type
declaration of

  fromIntegral :: (Num b, Integral a) => a -> b.

From its type signature we see that it takes an Integral number and turns it
into a more general number (Num). That's useful when you want integral and floating
point types to work together nicely.

For instance, the length function has a type declaration
of `length :: [a] -> Int` instead of having a more general type of (Num b) =>
length :: [a] -> b. Anyway, if we try to get a length of a list and then add it
to 3.2, we'll get an error because we tried to add together an Int and a
floating point number. So to get around this, we do
fromIntegral (length [1,2,3,4]) + 3.2 and it all works out.

* Note that `fromIntegral` has several class constraints in its type signature

Pattern matching
----------------

`lucky :: (Integral a) => a -> String`
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"
