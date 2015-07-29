-- Load in GHCI:
-- :l scratch.hs

doubleMe x = doubled
  where
    doubled = x * two
    two = 2

doubleUs :: Num t => t -> t -> [t]
doubleUs x y = [doubleMe x, doubleMe y]

triangles :: [(Integer, Integer, Integer)]
triangles = [ (a, b, c) | c <- [1..10], b <- [1..10], a <- [1..10] ]

rightTriangles = [ (a, b, c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]


addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

boomBangs xs = [ if x < 10 then "Boom!" else "Bang" | x <- xs ]

oddBoomBangs xs = [ if x < 10 then "Boom!" else "Bang" | x <- xs, odd x ]

-- Integral because that's what odd takes in
oddNumbers :: (Integral a) => [a] -> [a]
oddNumbers xs = filter odd xs

nouns = ["hobo","frog","pope"]
adjectives = ["lazy","grouchy","scheming"]

descriptiveWords = [ adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns ]

-- sum a 1 for each element in xs
length' xs = sum [1 | _ <- xs ]

-- Strings are just lists!
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, isUpper c]
  where isUpper c = elem c ['A'..'Z']