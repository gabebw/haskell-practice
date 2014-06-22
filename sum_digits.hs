-- Sum digits
-- http://www.reddit.com/r/dailyprogrammer/comments/1fnutb/06413_challenge_128_easy_sumthedigits_part_ii/
--
-- Example:
-- ./sum_digits
-- Enter a number> 12345
-- 12345
-- 15
-- 6

import System.IO (hFlush, stdout)

byTen :: Integral a => a -> (a, a)
byTen a = divMod a 10

splitDigits :: Integral a => a -> [a]
splitDigits t
  | t < 9 = [t]
  | otherwise = otherDigits ++ [lastDigit]
    where
      otherDigits = splitDigits firstDigits
      firstDigits = fst dividedByTen
      lastDigit = snd dividedByTen
      dividedByTen = byTen t

sumDigits :: Integral a => a -> a
sumDigits = sum . splitDigits

buildListOfSums :: Integral a => a -> [a]
buildListOfSums number
  | number < 10 = []
  | otherwise = [summed] ++ listOfSums
    where
      summed = sumDigits number
      listOfSums = buildListOfSums summed

main = do
  input <- askForInput
  let number = stringToInteger input
      array = buildListOfSums $ number
  putStrLn $ show number
  printOutEachElementIn $ array
  where
    printOutEachElementIn = mapM_ (putStrLn . show)
    stringToInteger = read
    askForInput = do
      putStr "Enter a number> "
      hFlush stdout
      getLine
