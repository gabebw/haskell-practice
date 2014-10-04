-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses

-- Doing `Point(..)` exports the value constructor, `Point` doesn't

module Shapes
( Point(..)
, Shape (..)
, surface
, nudge
, baseCircle
, baseRectangle
) where

-- Point is the name of the type and the value constructor
-- `deriving (Show)` automagically makes that type part of the Show typeclass
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- Note that surface takes a Shape - Circle is not a type, but Shape is
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) dx dy = Circle (Point (x+dx) (y+dy)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy = Rectangle p1 p2
  where
    p1 = Point (x1+dx) (y2+dy)
    p2 = Point (x2+dx) (y2+dy)

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRectangle :: Float -> Float -> Shape
baseRectangle width height = Rectangle (Point 0 0) (Point width height)

-- Record syntax!
-- now Haskell automatically gives us `flavor`, `firstName`, etc functions
-- Can create person two ways:
--    let person = Person "g" "bw" 72 34....etc
--  or
--    let person = Person {firstName="g", age=72, etc}
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

-- Maybe is a type constructor: it takes parameters (`a`) to produce new types
--The value `Just 'a'` has a type of `Maybe Char`
--The value `Nothing` has a type of `Maybe a`
-- Nothing and Just are value constructors
data Maybe a = Nothing | Just a

-- `type` makes synonyms, it doesn't define new types
-- type String = [Char]

-- Tuesday > Monday => True
-- minBound :: Dady => Monday } due to Bounded typeclass
-- maxBound :: Dady => Sunday }
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)


-- Recursive constructor
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- Custom infix operator
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right
