-- Reads a filename from STDIN and prints its contents
-- Usage: echo girlfriend.txt | runhaskell easy_read_file.txt
import System.IO

main = do
  fileName <- getLine
  contents <- readFile fileName
  putStr contents
