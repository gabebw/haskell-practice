-- Reverse a list

reverse' = reverse

reverse'' [] = []
reverse'' (x:xs) = reverse'' xs ++ [x]
