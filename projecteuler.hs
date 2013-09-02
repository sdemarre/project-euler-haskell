
problem_1_n n = sum (filter (\x -> or [mod x 3 == 0, mod x 5 == 0]) [1..n])
problem_1 = problem_1_n 999

fib_rec :: (Integral a) => a -> [a] -> a
fib_rec _ [] = error "wrong fib_rec call, empty rec"
fib_rec _ [_] = error "wrong fib_rec call, single element rec"
fib_rec 0 (x:y:xs) = y
fib_rec 1 (x:xs) = x
fib_rec n all@(x:y:xs) = fib_rec (n - 1) ([x+y] ++ all)

fib :: (Integral a) => a -> a
fib x = fib_rec x [1,0]

problem_2_n n = sum (takeWhile (< n) (filter even (map fib [0..])))
problem_2 = problem_2_n 4000000

isDivisor :: (Integral a) => a -> a -> Bool
isDivisor x y = mod y x == 0

isPrime :: (Integral a) => a -> Bool
isPrime x = length (divisors x) == 0

divisors :: (Integral a) => a->[a]
divisors x = divisor_list x 2 []

divisor_list :: (Integral a) => a -> a -> [a] -> [a]
divisor_list n start list
  | (start*start > n) = n:list
  | (mod n start) == 0 = divisor_list (div n start) 2 [start] ++ list
  | otherwise = divisor_list n (1 + start) list

problem_3_n n = head $ divisors n
problem_3 = problem_3_n 600851475143

isStringPalindrome :: [Char] -> Bool
isStringPalindrome xs = isStringPalindrome_list xs ((length xs) `div` 2) []

isStringPalindrome_list :: (Integral b) => [Char] -> b -> [Char] -> Bool
isStringPalindrome_list [] 0 _ = True
isStringPalindrome_list (x:xs) len allOut@(y:ys)
  | len == 0 = and [x == y, isStringPalindrome_list xs 0 ys]
  | otherwise = isStringPalindrome_list xs (len - 1) (x:allOut)
isStringPalindrome_list _ _ _ = False

