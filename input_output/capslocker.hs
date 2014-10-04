import Data.Char

main = interact shortLinesOnly
-- `interact shortLinesOnly` is just like doing this:
-- do
--    contents <- getContents
--    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input =
  unlines shortLines
  where
    shortLines = filter (\line -> length line < 10) allLines
    allLines = lines input
