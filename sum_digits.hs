-- Sum digits
-- > 12345
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

stringToInteger = read

buildListOfSums :: Integral a => a -> [a]
buildListOfSums number
  | number < 10 = []
  | otherwise = [summed] ++ listOfSums
    where
      summed = sumDigits number
      listOfSums = buildListOfSums summed

printOutEachElementIn = mapM_ (putStrLn . show)

main = do
  putStr "Enter a number> "
  hFlush stdout
  number <- getLine
  putStrLn number
  let array = buildListOfSums $ stringToInteger number
  printOutEachElementIn array
