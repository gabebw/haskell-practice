-- Disemvowel
-- http://www.reddit.com/r/dailyprogrammer/comments/1ystvb/022414_challenge_149_easy_disemvoweler/
-- Input: a string, all lowercase, no punctuation
-- Output: two strings, one of them disemvoweled and one of them with only
-- vowels. Neither should have spaces.

import System.IO (hFlush, stdout)

notSpace :: Char -> Bool
notSpace = not . isSpace
  where
    isSpace = (`elem` " ")

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")

withoutVowels :: String -> String
withoutVowels s = [ l | l <- s, not $ isVowel l, notSpace l ]

onlyVowels :: String -> String
onlyVowels s = [ l | l <- s, isVowel l, notSpace l ]

main = do
  input <- askForInput
  putStrLn ""
  putStrLn input
  putStrLn $ "  No vowels: "++withoutVowels input
  putStrLn $ "Only vowels: "++onlyVowels input
  where
    askForInput = do
      putStr "Enter a string to disemvowel> "
      hFlush stdout
      getLine
