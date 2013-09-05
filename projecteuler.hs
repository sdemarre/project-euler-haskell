import qualified Data.Char
import qualified Data.Numbers.Primes

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

problem_9 = [[a,b,c] | a<-[1..1000], b<-[a+1..1000], c<-[1000-a-b], a^2+b^2==c^2]

problem_10_n n = 2 + (foldr1 (+) (takeWhile (\x -> x < n) Data.Numbers.Primes.primes))
problem_10 = problem_10_n 2000000

problem_11_data = [[08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08],
                   [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00],
                   [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65],
                   [52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91],
                   [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80],
                   [24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50],
                   [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70],
                   [67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21],
                   [24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72],
                   [21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95],
                   [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92],
                   [16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57],
                   [86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58],
                   [19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40],
                   [04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66],
                   [88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69],
                   [04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36],
                   [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16],
                   [20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54],
                   [01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]]
p11_data x y = problem_11_data !! y !! x

p11_elements xorig yorig [] = []
p11_elements xorig yorig (deltax:deltay:deltas) = [p11_data (xorig + deltax) (yorig + deltay)] ++ (p11_elements xorig yorig deltas)
p11_4_horizontal xorig yorig = p11_elements xorig yorig [0,0, 1,0, 2,0, 3,0]
p11_4_vertical xorig yorig = p11_elements xorig yorig [0,0, 0,1, 0,2, 0,3]
p11_4_tlbr xorig yorig = p11_elements xorig yorig [0,0, 1,1, 2,2, 3,3]
p11_4_trbl xorig yorig = p11_elements xorig yorig [0,0, -1,1, -2,2, -3,3]


p11_horizontals = map (\ [x,y]->p11_4_horizontal x y) [[x,y] | x<-[0..16], y<-[0..19]]
p11_verticals = map (\ [x,y]->p11_4_vertical x y) [[x,y] | x<-[0..19], y<-[0..16]]
p11_tlbrs = map (\ [x,y]->p11_4_tlbr x y) [[x,y] | x<-[0..16], y<-[0..16]]
p11_trbls = map (\ [x,y]->p11_4_trbl x y) [[x,y] | x<-[3..19], y<-[0..16]]

problem_11 = maximum (map product $ p11_horizontals ++ p11_verticals ++ p11_tlbrs ++ p11_trbls)

triangleNumber n = div (n*(n+1)) 2


groupSizes [] = []
groupSizes all@(x:xs) = groupSizes_list x all 0 []

groupSizes_list _ [] 0 result = reverse result
groupSizes_list _ [] n result = reverse (n:result)
groupSizes_list y (x:xs) count result = if y == x then
                                          groupSizes_list x xs (1+count) result
                                        else
                                          groupSizes_list x xs 1 (count:result)
numberDivisors n = product (map (+1) (groupSizes (Data.Numbers.Primes.primeFactors n)))

problem_12_n n = let number = head(reverse(takeWhile (\ i -> (numberDivisors (triangleNumber i)) <= n) [1..]))
                 in triangleNumber (1+number)
problem_12
