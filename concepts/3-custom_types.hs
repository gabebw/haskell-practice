-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses

module Shapes where

-- Here, Point is the name of the type *and* the value constructor
-- `deriving (Show)` automagically makes that type part of the Show typeclass
data Point = Point Float Float deriving (Show)

-- Here, Shape is the name of the type,
-- but `Circle` and `Rectangle` are the value constructors.
-- So a function can't have `Circle` in its type signature, but it can have
-- `Shape`.
data Shape =
    Circle Point Float
    | Rectangle Point Point
    deriving (Show)

-- Note that surface takes a Shape - Circle is not a type, but Shape is
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- nudge a shape by the given x/y amounts
-- Lots of pattern matching!
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
--    Person "g" "bw" 72 34....etc
--  or
--    Person {firstName="g", age=72, etc}
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

-- Maybe is a type constructor: it takes parameters (`a`) to produce new types
-- The value `Just "a"` has a type of `Maybe String`
-- The value `Nothing` has a type of `Maybe a`
--
-- Nothing and Just are value constructors
data Maybe a = Nothing | Just a

-- `type` makes synonyms, it doesn't define new types
-- type String = [Char]

-- Tuesday > Monday => True
data Day =
    Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- `Bounded` means we can use minBound/maxBound
earliestDay :: Day
earliestDay = minBound

latestDay :: Day
latestDay = maxBound

-- We can do `..` due to the `Enum` typeclass
allDays :: [Day]
allDays = [Monday .. Sunday]

data TrafficLight = Red | Yellow | Green

-- Now TrafficLight is an instance of the Eq typeclass
-- We could also get this for free with `deriving Eq`.
instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

-- Now TrafficLight is an instance of the Show typeclass
instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"
