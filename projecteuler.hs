import qualified Data.Char

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
isPrime x = length (primeFactors x) == 1

primeFactors :: (Integral a) => a->[a]
primeFactors x = primeFactors_list x 2 []

primeFactors_list :: (Integral a) => a -> a -> [a] -> [a]
primeFactors_list n start list
  | (start*start > n) = n:list
  | (mod n start) == 0 = primeFactors_list (div n start) 2 [start] ++ list
  | otherwise = primeFactors_list n (1 + start) list

problem_3_n n = head $ primeFactors n
problem_3 = problem_3_n 600851475143

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = isPalindrome_list xs (div (length xs) 2) []

isPalindrome_list :: (Eq a, Integral b) => [a] -> b -> [a] -> Bool
isPalindrome_list [] 0 _ = True
isPalindrome_list (x:xs) 0 (y:ys) = and [x == y, isPalindrome_list xs 0 ys]
isPalindrome_list (x:xs) len ys = isPalindrome_list xs (len - 1) (x:ys)
isPalindrome_list _ _ _ = False

isPalindromeNumber = isPalindrome . numberToDigits

numberToDigits :: (Integral a) => a -> [a]
numberToDigits n = numberToDigits_list n []

digitsToNumber :: (Integral a) => [a] -> a
digitsToNumber xs = digitsToNumber_list 0 xs
digitsToNumber_list :: (Integral a) => a -> [a] -> a
digitsToNumber_list n [] = n
digitsToNumber_list n (x:xs) = digitsToNumber_list (10*n+x) xs


numberToDigits_list :: (Integral a) => a -> [a] -> [a]
numberToDigits_list 0 all@(x:xs) = all
numberToDigits_list 0 [] = [0]
numberToDigits_list n xs = numberToDigits_list (div n 10) ((mod n 10):xs)

concatLists = foldr (++) []
problem_4 = let candidates = filter isPalindromeNumber $ concatLists (map (\x-> map (*x) [x..999]) [100..999])
            in
             maximum candidates

problem_5_n n = foldr lcm 1 [1..n]
problem_5 = problem_5_n 20

problem_6_n :: (Integral a) => a -> a
problem_6_n n = div ((n-1)*n*(n+1)*(3*n+2)) 12
problem_6 = problem_6_n 100

problem_7_n n = head $ reverse (take n (filter isPrime [2..]))
problem_7 = problem_7_n 10001


multiplyNumbers :: (Integral a) => a -> [a] -> a
multiplyNumbers 0 _ = 0
multiplyNumbers 1 (x:xs) = x
multiplyNumbers n (x:xs) = x * (multiplyNumbers (n - 1) xs)
multiplyNumbers _ _ = 0

mapList :: ([a] -> b) -> [a] -> [b]
mapList f [] = []
mapList f xs = ((f xs):(mapList f (tail xs)))

problem_8_data = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450
problem_8 = maximum (mapList (\n -> multiplyNumbers 5 n) (numberToDigits problem_8_data))
