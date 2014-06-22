-- 1: Find the last element of a list

last1 = last

last2 = head . reverse

-- Oh right, pattern matching
last3 [x] = x
last3 (x:xs) = last3 xs
