module WordCountExercise where

import Data.List (find, delete, sortOn)
import Control.Monad ((>=>))
import System.Environment (getArgs)

data Wordcount = Wordcount String Int deriving (Eq, Show)

-- Count how many times each string appears in the list.
count :: [String] -> [Wordcount]
count (s:ss) = foldl findOrIncrementFrom [baseCount s] ss

-- Given a wordcount, return a new one with the count increased by 1
inc :: Wordcount -> Wordcount
inc (Wordcount w c) = Wordcount w (c+1)

-- If a Wordcount whose word is `s` is in `wcs`, increment its count.
-- Otherwise, stick on a new Wordcount for the word with a count of 1.
findOrIncrementFrom :: [Wordcount] -> String -> [Wordcount]
findOrIncrementFrom wcs s = case findWordCountByWord s wcs of
    (Just wc) ->  (inc wc):(delete wc wcs)
    Nothing -> (baseCount s):wcs

-- Possibly find a Wordcount in `wcs` whose word is `s`
findWordCountByWord :: String -> [Wordcount] -> Maybe Wordcount
findWordCountByWord w wcs = find matchOnWord wcs
    where
        matchOnWord (Wordcount w' _) = w' == w

baseCount :: String -> Wordcount
baseCount s = Wordcount s 1

-- Usage: runhaskell Wordcount.hs file1 file2 ...
--
-- For each filename passed to this program, it will print out the filename then
-- print out the top 5 most common words for that file.
main = getArgs >>= mapM (printAndRead >=> (print . top5 . count . words))
    where
        top5 = take 5 . sortOn reverseCompare
        reverseCompare (Wordcount _ c) = c * (-1)
        printAndRead s = putStrLn ("\n"++s++"\n") >> readFile s
