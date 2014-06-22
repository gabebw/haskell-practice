-- 2: Find the second-to-last element of a list

secondToLast = last . init
secondToLast' l = reverse l !! 1

secondToLast'' [a,_] = a
secondToLast'' (_:xs) = secondToLast'' xs

secondToLast''' = head . tail . reverse

