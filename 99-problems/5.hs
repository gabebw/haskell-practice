-- Reverse a list

-- Duh.
reverse' = reverse

reverse'' [] = []
reverse'' (x:xs) = reverse' xs ++ [x]
