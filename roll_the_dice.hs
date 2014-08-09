-- http://www.reddit.com/r/dailyprogrammer/comments/1givnn/061713_challenge_130_easy_roll_the_dies/

-- Sample Input
--  2d20
--  4d6
-- Sample Output
--  19 7
--  5 3 4 6

import System.IO (hFlush, stdout)
import System.Random
import Data.Text (splitOn, pack, unpack, Text)
import Data.List (intercalate)

splitOnD :: String -> [String]
splitOnD s = map unpack $ splitOn (pack "d") (pack s)

generator = mkStdGen 35352341

numberOfRolls :: String -> Int
numberOfRolls s = read $ split !! 0
  where
    split = splitOnD s

faces :: String -> Int
faces s = read $ split !! 1
  where
    split = splitOnD s

joinWithSpaces :: [String] -> String
joinWithSpaces = intercalate " "

results :: String -> [Int]
results s = take (numberOfRolls s) rollResults
  where
    rollResults = randomRs (1, (faces s)) generator

main = do
  input <- askForInput
  putStr $ joinWithSpaces $ map show $ results input
  putStrLn ""
  where
    askForInput = do
      putStr "Enter 3d6, etc> "
      hFlush stdout
      getLine
