-- Eliminate consecutive duplicates of list elements.
-- > compress "aaaabccaadeeee"
-- "abcade"

module Compress where

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = foldl addUnlessLatest [x] xs
  where
    addUnlessLatest acc element = if element == (last acc) then acc else acc ++ [element]
