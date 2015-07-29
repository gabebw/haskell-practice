-- Disemvowel
-- http://www.reddit.com/r/dailyprogrammer/comments/1ystvb/022414_challenge_149_easy_disemvoweler/
-- Input: a string, all lowercase, no punctuation
-- Output: two strings, one of them disemvoweled and one of them with only
-- vowels. Neither should have spaces.

import System.IO (hFlush, stdout)

notSpace :: Char -> Bool
notSpace = (/= ' ')

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")

withoutVowels :: String -> String
withoutVowels = filter (not .isVowel)

onlyVowels :: String -> String
onlyVowels = filter isVowel

main = do
  input <- askForInput
  putStrLn ""
  putStrLn input
  putStrLn $ "  No vowels: " ++ (filter notSpace $ withoutVowels input)
  putStrLn $ "Only vowels: " ++ (filter notSpace $ onlyVowels input)
  where
    askForInput = do
      putStr "Enter a string to disemvowel> "
      hFlush stdout
      getLine
