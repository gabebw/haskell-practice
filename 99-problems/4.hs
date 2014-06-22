-- Find the number of elements of a list.

length' = length

length'' xs = sum [ 1 | x <- xs ]

length''' = sum . map (\_ -> 1)

length'''' [] = 0
length'''' (_:xs) = 1 + length''' xs
