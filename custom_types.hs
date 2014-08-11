-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses

-- Point is the name of the type and the value constructor
-- `deriving (Show)` automagically makes that type part of the Show typeclass
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- Note that surface takes a Shape - Circle is not a type, but Shape is
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
