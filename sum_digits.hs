-- Sum digits
-- > 12345
-- 15
-- 6

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

main = do
  putStrLn "Enter a number > "
  number <- getLine
  putStrLn "---- Sums:"
  putStrLn number
  let array = map show $ buildListOfSums $ stringToInteger number
  mapM_ putStrLn array
