import System.IO
import Data.Char

main = do
  contents <- readFile "girlfriend.txt"
  writeFile "UPPER_CASE_GIRLFRIEND.TXT" (map toUpper contents)
