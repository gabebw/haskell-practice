-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses

-- Circle is a constructor that takes three floats
-- Rectangle is a constructor that takes four floats
-- `deriving (Show)` automagically makes that type part of the Show typeclass
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- Note that surface takes a Shape - Circle is not a type, but Shape is
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
