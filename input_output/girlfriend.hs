import System.IO

main = do
  -- handle <- openFile "girlfriend.txt" ReadMode
  withFile "girlfriend.txt" ReadMode printLines

printLines :: Handle -> IO ()
printLines handle = do
  contents <- hGetContents handle
  print $ lines contents
