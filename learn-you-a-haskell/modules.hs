-- Grab only `nub` and `sort` from Data.List
import Data.List (nub, sort)
-- Import everything EXCEPT `nub`
-- import Data.List hiding (nub)

-- Qualified import - good for when function names conflict
-- Now do `M.nub` instead of `nub`
-- import qualified Data.List as M (nub)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
