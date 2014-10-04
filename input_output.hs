import Data.Char

-- Use `do` syntax to glue together a bunch of IO actions
main = do
  putStrLn "Hello, what's your name?"
  -- This line performs the IO action getLine and binds its result to `name`
  -- The `<-` pulls the String out of the `IO String` that `getLine` returns
  name <- getLine
  -- bind a non-IO result
  let bigName = map toUpper name
  putStrLn ("Hey " ++ bigName ++ ", you rock!")
