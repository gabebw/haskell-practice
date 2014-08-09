-- map a map!
-- map (map (^2)) [[1,2],[3,4,5,6],[7,8]]


-- Hey, guards are back
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

-- Parentheses in type declaration are mandatory, because it's a function
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- (a -> b -> c) is a function that takes two arguments of type a and b and
-- returns something of type c
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x, y)] ++ zip' xs ys

-- Take a function that takes a and b and returns c, then
-- return a functiont aht takes b and a and returns c
flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

-- :)
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

collatz :: (Integral a) => a => [a]
collatz 1 = [1]
collatz n
  | odd n = n:collatz (n * 3 + 1)
  | even n = n:collatz (n `div` 2)

-- Let's use ~FOLDS~
maximum' :: (Ord a) => [a] -> a
maximum' (x:xs) = foldl isBigger x xs
  where isBigger acc x = if x > acc then x else acc

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldl1 isBigger
  where isBigger acc x = if x > acc then x else acc

maximum''' :: (Ord a) => [a] -> a
maximum''' = foldr1 isBigger
  where isBigger x acc = if x > acc then x else acc

reverse' :: (Ord a) => [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

sum' :: (Num a) => [a] -> a
sum' = foldl1 (+)

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

-- scanl/scanr show intermediate steps of foldl/foldr

-- Function composition: f . g

-- Point-free style: leave off argument
withPoint x = ceiling . negate . tan . cos . max 50 x
pointFree = ceiling . negate . tan . cos . max 50
