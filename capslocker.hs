import Data.Char

main = do
  contents <- getContents
  putStrLn (map toUpper contents)

shortLinesOnly :: String -> String
shortLinesOnly input =
  unlines shortLines
  where
    shortLines = filter (\line -> length line < 10) allLines
    allLines = lines input
