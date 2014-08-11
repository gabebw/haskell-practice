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
