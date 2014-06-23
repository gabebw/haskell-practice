-- Find out whether a list is a palindrome.

isPalindrome xs = reverse xs == xs

isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' xs = firstMatchesLast && (isPalindrome' middle)
  where
    firstMatchesLast = head xs == last xs
    middle = init $ tail xs
