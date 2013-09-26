import qualified Data.Numbers.Primes as Primes
import qualified Data.Map.Strict as Map
import qualified Data.Maybe
import qualified Debug.Trace
import qualified Data.Char
import qualified Data.List
import qualified Data.Set
import Data.Time

problem1N n = sum (filter (\x -> (mod x 3 == 0) || (mod x 5 == 0)) [1..n])
problem1 = problem1N 999

fib :: (Integral a) => a -> a
fib x = let fib_rec :: (Integral a) => a -> [a] -> a
            fib_rec _ [] = error "wrong fib_rec call, empty rec"
            fib_rec _ [_] = error "wrong fib_rec call, single element rec"
            fib_rec 0 (x:y:xs) = y
            fib_rec 1 (x:xs) = x
            fib_rec n all@(x:y:xs) = fib_rec (n - 1) ((x+y) : all)
        in fib_rec x [1,0]

problem2N n = sum (takeWhile (< n) (filter even (map fib [0..])))
problem2 = problem2N 4000000

isDivisor :: (Integral a) => a -> a -> Bool
isDivisor x y = mod y x == 0

isPrime :: (Integral a) => a -> Bool
isPrime x = length (primeFactors x) == 1

primeFactors :: (Integral a) => a->[a]
primeFactors x = let primeFactors_list :: (Integral a) => a -> a -> [a] -> [a]
                     primeFactors_list n start list
                       | (start*start > n) = n:list
                       | mod n start == 0 = primeFactors_list (div n start) 2 [start] ++ list
                       | otherwise = primeFactors_list n (1 + start) list
                 in primeFactors_list x 2 []


problem3N n = head $ primeFactors n
problem3 = problem3N 600851475143

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = let isPalindrome_rec :: (Eq a, Integral b) => [a] -> b -> [a] -> Bool
                      isPalindrome_rec [] 0 _ = True
                      isPalindrome_rec (x:xs) 0 (y:ys) = ((x == y) && isPalindrome_rec xs 0 ys)
                      isPalindrome_rec (x:xs) len ys = isPalindrome_rec xs (len - 1) (x:ys)
                      isPalindrome_rec _ _ _ = False
                  in isPalindrome_rec xs (div (length xs) 2) []

isPalindromeNumber = isPalindrome . numberToDigits

numberToDigits :: (Integral a) => a -> [a]
numberToDigits n = let numberToDigits_rec :: (Integral a) => a -> [a] -> [a]
                       numberToDigits_rec 0 all@(x:xs) = all
                       numberToDigits_rec 0 [] = [0]
                       numberToDigits_rec n xs = numberToDigits_rec (div n 10) (mod n 10 : xs)
                   in numberToDigits_rec n []

digitsToNumber :: (Integral a) => [a] -> a
digitsToNumber = foldl (\ n x -> 10*n+x) 0

problem4 = let candidates = filter isPalindromeNumber $ concatMap (\x-> map (*x) [x..999]) [100..999]
           in maximum candidates

problem5N n = foldr lcm 1 [1..n]
problem5 = problem5N 20

problem6N :: (Integral a) => a -> a
problem6N n = div ((n-1)*n*(n+1)*(3*n+2)) 12
problem6 = problem6N 100

problem7N n = last (take n (filter isPrime [2..]))
problem7 = problem7N 10001


multiplyNumbers :: (Integral a) => a -> [a] -> a
multiplyNumbers 0 _ = 0
multiplyNumbers 1 (x:xs) = x
multiplyNumbers n (x:xs) = x * multiplyNumbers (n - 1) xs
multiplyNumbers _ _ = 0

mapList :: ([a] -> b) -> [a] -> [b]
mapList f [] = []
mapList f xs = f xs : mapList f (tail xs)

problem8Data = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450
problem8 = maximum (mapList (multiplyNumbers 5) (numberToDigits problem8Data))

problem9 = [[a,b,c] | a<-[1..1000], b<-[a+1..1000], c<-[1000-a-b], a^2+b^2==c^2]

problem10N n = 2 + sum (takeWhile (<n) Primes.primes)
problem10 = problem10N 2000000

problem11Data = [[08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08],
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

p11Data x y = problem11Data !! y !! x

p11Elements xorig yorig [] = []
p11Elements xorig yorig (deltax:deltay:deltas) = p11Data (xorig + deltax) (yorig + deltay) : p11Elements xorig yorig deltas
p11Horizontal4 xorig yorig = p11Elements xorig yorig [0,0, 1,0, 2,0, 3,0]
p11Vertical4 xorig yorig = p11Elements xorig yorig [0,0, 0,1, 0,2, 0,3]
p11Tlbr4 xorig yorig = p11Elements xorig yorig [0,0, 1,1, 2,2, 3,3]
p11Trbl4 xorig yorig = p11Elements xorig yorig [0,0, -1,1, -2,2, -3,3]


p11Horizontals = map (\ [x,y] -> p11Horizontal4 x y) [[x,y] | x<-[0..16], y<-[0..19]]
p11Verticals = map (\ [x,y] -> p11Vertical4 x y) [[x,y] | x<-[0..19], y<-[0..16]]
p11Tlbrs = map (\ [x,y] -> p11Tlbr4 x y) [[x,y] | x<-[0..16], y<-[0..16]]
p11Trbls = map (\ [x,y] -> p11Trbl4 x y) [[x,y] | x<-[3..19], y<-[0..16]]

problem11 = maximum (map product $ p11Horizontals ++ p11Verticals ++ p11Tlbrs ++ p11Trbls)

triangleNumber n = div (n*(n+1)) 2


groupSizes [] = []
groupSizes all@(x:xs) = let groupSizes_list _ [] 0 result = reverse result
                            groupSizes_list _ [] n result = reverse (n:result)
                            groupSizes_list y (x:xs) count result
                              | (y == x) = groupSizes_list x xs (1+count) result
                              | otherwise = groupSizes_list x xs 1 (count:result)
                        in  groupSizes_list x all 0 []

numberDivisors n = product (map (+1) (groupSizes (Primes.primeFactors n)))

problem12N n = let number = last (takeWhile (\ i -> numberDivisors (triangleNumber i) <= n) [1..])
               in triangleNumber (1+number)
problem12 = problem12N 500

problem13Data = [37107287533902102798797998220837590246510135740250,
                 46376937677490009712648124896970078050417018260538,
                 74324986199524741059474233309513058123726617309629,
                 91942213363574161572522430563301811072406154908250,
                 23067588207539346171171980310421047513778063246676,
                 89261670696623633820136378418383684178734361726757,
                 28112879812849979408065481931592621691275889832738,
                 44274228917432520321923589422876796487670272189318,
                 47451445736001306439091167216856844588711603153276,
                 70386486105843025439939619828917593665686757934951,
                 62176457141856560629502157223196586755079324193331,
                 64906352462741904929101432445813822663347944758178,
                 92575867718337217661963751590579239728245598838407,
                 58203565325359399008402633568948830189458628227828,
                 80181199384826282014278194139940567587151170094390,
                 35398664372827112653829987240784473053190104293586,
                 86515506006295864861532075273371959191420517255829,
                 71693888707715466499115593487603532921714970056938,
                 54370070576826684624621495650076471787294438377604,
                 53282654108756828443191190634694037855217779295145,
                 36123272525000296071075082563815656710885258350721,
                 45876576172410976447339110607218265236877223636045,
                 17423706905851860660448207621209813287860733969412,
                 81142660418086830619328460811191061556940512689692,
                 51934325451728388641918047049293215058642563049483,
                 62467221648435076201727918039944693004732956340691,
                 15732444386908125794514089057706229429197107928209,
                 55037687525678773091862540744969844508330393682126,
                 18336384825330154686196124348767681297534375946515,
                 80386287592878490201521685554828717201219257766954,
                 78182833757993103614740356856449095527097864797581,
                 16726320100436897842553539920931837441497806860984,
                 48403098129077791799088218795327364475675590848030,
                 87086987551392711854517078544161852424320693150332,
                 59959406895756536782107074926966537676326235447210,
                 69793950679652694742597709739166693763042633987085,
                 41052684708299085211399427365734116182760315001271,
                 65378607361501080857009149939512557028198746004375,
                 35829035317434717326932123578154982629742552737307,
                 94953759765105305946966067683156574377167401875275,
                 88902802571733229619176668713819931811048770190271,
                 25267680276078003013678680992525463401061632866526,
                 36270218540497705585629946580636237993140746255962,
                 24074486908231174977792365466257246923322810917141,
                 91430288197103288597806669760892938638285025333403,
                 34413065578016127815921815005561868836468420090470,
                 23053081172816430487623791969842487255036638784583,
                 11487696932154902810424020138335124462181441773470,
                 63783299490636259666498587618221225225512486764533,
                 67720186971698544312419572409913959008952310058822,
                 95548255300263520781532296796249481641953868218774,
                 76085327132285723110424803456124867697064507995236,
                 37774242535411291684276865538926205024910326572967,
                 23701913275725675285653248258265463092207058596522,
                 29798860272258331913126375147341994889534765745501,
                 18495701454879288984856827726077713721403798879715,
                 38298203783031473527721580348144513491373226651381,
                 34829543829199918180278916522431027392251122869539,
                 40957953066405232632538044100059654939159879593635,
                 29746152185502371307642255121183693803580388584903,
                 41698116222072977186158236678424689157993532961922,
                 62467957194401269043877107275048102390895523597457,
                 23189706772547915061505504953922979530901129967519,
                 86188088225875314529584099251203829009407770775672,
                 11306739708304724483816533873502340845647058077308,
                 82959174767140363198008187129011875491310547126581,
                 97623331044818386269515456334926366572897563400500,
                 42846280183517070527831839425882145521227251250327,
                 55121603546981200581762165212827652751691296897789,
                 32238195734329339946437501907836945765883352399886,
                 75506164965184775180738168837861091527357929701337,
                 62177842752192623401942399639168044983993173312731,
                 32924185707147349566916674687634660915035914677504,
                 99518671430235219628894890102423325116913619626622,
                 73267460800591547471830798392868535206946944540724,
                 76841822524674417161514036427982273348055556214818,
                 97142617910342598647204516893989422179826088076852,
                 87783646182799346313767754307809363333018982642090,
                 10848802521674670883215120185883543223812876952786,
                 71329612474782464538636993009049310363619763878039,
                 62184073572399794223406235393808339651327408011116,
                 66627891981488087797941876876144230030984490851411,
                 60661826293682836764744779239180335110989069790714,
                 85786944089552990653640447425576083659976645795096,
                 66024396409905389607120198219976047599490197230297,
                 64913982680032973156037120041377903785566085089252,
                 16730939319872750275468906903707539413042652315011,
                 94809377245048795150954100921645863754710598436791,
                 78639167021187492431995700641917969777599028300699,
                 15368713711936614952811305876380278410754449733078,
                 40789923115535562561142322423255033685442488917353,
                 44889911501440648020369068063960672322193204149535,
                 41503128880339536053299340368006977710650566631954,
                 81234880673210146739058568557934581403627822703280,
                 82616570773948327592232845941706525094512325230608,
                 22918802058777319719839450180888072429661980811197,
                 77158542502016545090413245809786882778948721859617,
                 72107838435069186155435662884062257473692284509516,
                 20849603980134001723930671666823555245252804609722,
                 53503534226472524250874054075591789781264330331690]

problem13 = digitsToNumber $ take 10 $ numberToDigits $ sum problem13Data

collatz n = let collatz_rec n xs
                  | n == 1 = (1:xs)
                  | even n = collatz_rec (div n 2) (n:xs)
                  | otherwise = collatz_rec (3*n+1) (n:xs)
            in reverse $ collatz_rec n []

collatzStep n
    | even n = div n 2
    | otherwise = 3*n+1

collatzLength n
  | n == 1 = 1
  | even n = 1 + collatzLength (div n 2)
  | otherwise = 1 + collatzLength (3*n+1)

-- doesn't work for high (>100000) limits, too slow or memory exhausted
problem14Fails limit = foldl1 (\ l@(_:ll) r@(_:lr) -> if ll > lr then l else r)
                       $ map (\ n -> n: [length $ collatz n]) [1..limit]

incrementalCollatzCount counts n
  | Data.Maybe.isJust ( Map.lookup n counts ) = counts
  | otherwise = let newCounts = incrementalCollatzCount counts (collatzStep n)
                    steps = 1 + Data.Maybe.fromJust (Map.lookup (collatzStep n) newCounts)
                in Map.insert n steps newCounts

problem14N limit = let m = foldl incrementalCollatzCount (Map.fromList [(1,1)]) [1..limit]
                   in Map.foldlWithKey (\currentBest number collatzLength -> if snd currentBest < collatzLength then (number, collatzLength) else currentBest) (0,0) m

problem14 = problem14N 1000000

extend15 d = [[elem d i j | i<-[0..length d]] | j<-[0..length d]]
  where elem d i j
          | i > 0 && j > 0 = d !! (i-1) !! (j-1)
          | i == length d || j == length d = 1
          | otherwise = elem d (i+1) j + elem d i (j+1)

problem15N n = head $ head (p15 [[1]] n)
  where p15 d 0 = d
        p15 d n = p15 (extend15 d) (n-1)

problem15 = problem15N 20

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturdat | Sunday deriving (Eq, Show, Ord, Bounded, Enum)
nextDayOfWeek dayOfWeek = if dayOfWeek == (maxBound :: DayOfWeek)
                          then (minBound :: DayOfWeek)
                          else succ dayOfWeek
data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Eq, Show, Ord, Bounded, Enum)
nextMonth month = if month == (maxBound :: Month)
                  then (minBound :: Month)
                  else succ month
monthLength month year
  | month == February = if Data.Time.isLeapYear year then 29 else 28
  | month `elem` [April, June, September, November] = 30
  | otherwise = 31
data Day = Day Integer Month Integer deriving (Eq, Show, Ord)
nextDay (Day year month day) = if day == monthLength month year
                                  then if month == (maxBound :: Month)
                                       then Day (year + 1) (minBound :: Month) 1
                                       else Day year (nextMonth month) 1
                               else Day year month (day + 1)

daysInRangeWhen firstDay startDayOfWeek lastDay whenToCount
  | firstDay > lastDay = []
  | whenToCount firstDay startDayOfWeek =  (firstDay, startDayOfWeek):daysInRangeWhen (nextDay firstDay) (nextDayOfWeek startDayOfWeek) lastDay whenToCount
  | otherwise = daysInRangeWhen (nextDay firstDay) (nextDayOfWeek startDayOfWeek) lastDay whenToCount

problem19 = length $ daysInRangeWhen firstDay firstDayOfWeek lastDay filter
            where firstDay = Day 1900 January 1
                  firstDayOfWeek = Monday
                  lastDay = Day 2000 December 31
                  filter theDay@(Day year month day) dayOfWeek  =
                    theDay > Day 1900 December 31 && day == 1 && dayOfWeek == Sunday

problem20N n = sum $ numberToDigits $ product [1..n]
problem20 = problem20N 100

removeDuplicates xs = Map.keys $ Map.fromList [(x,1)|x<-xs]

divisorList n = removeDuplicates $ map (composeNumber factors) [0..2^length factors - 2]
                where factors = Primes.primeFactors n

composeNumber [] _ = 1
composeNumber all@(x:xs) combination
  | combination `mod` 2 == 0 = composeNumber xs (combination `div` 2)
  | otherwise = x * composeNumber xs (combination `div` 2)

divisorSum n = sum $ divisorList n

problem21 = sum $ filter (\x->(divisorSum . divisorSum) x == x && not (x == divisorSum x)) [2..10000]


letterValue c = 1 + Data.Char.ord c - Data.Char.ord 'A'
wordValue word = sum $ map letterValue word
problem22WithData p22data = sum $ zipWith (\word pos->wordValue word * pos) (Data.List.sort p22data) [1..]

isAbundandNumber n = n < (sum $ divisorList n)
problem23 = Data.Set.foldl (+) 0 $ Data.Set.difference (Data.Set.fromList [1..limit - 1]) (Data.Set.fromList sums)
  where sums = [((anums !! x)+(anums !! y)) |
                    x<-[0..(length anums) - 1],y<-[x..(length anums) - 1],
                    ((anums !! x)+(anums !! y)) < limit]
        limit = 28123
        anums = filter isAbundandNumber [1..limit - 1]

nthCombination _ [] = []
nthCombination combidx source = (source !! idx):(nthCombination (combidx `mod` f) $ Data.List.delete (source !! idx) source)
  where idx = combidx `div` f
        fact n = product [1..n]
        f = fact ((length source) - 1)
problem24 = nthCombination 1000000 [0..9]

numDigits n = length $ show n
problem25N n = fibUntilLength_rec n [1,1]
  where
    fibUntilLength_rec n fibList@(fn:fnm1:_) = if ((numDigits fn) >= n)
                                               then length fibList
                                               else fibUntilLength_rec n ((fn+fnm1):fibList)

problem25 = problem25N 1000

divStep :: (Integral a) => a->a->[a]
divStep a b = [q,(a-b*q)*10]
  where q = div a b
addDivStep dividend divisor divSteps = [newDividend,quotient]:divSteps
  where [quotient,newDividend] = divStep dividend divisor
extendDivStepUntilLoop dividend divisor divSteps =
  let [quotient,newDividend] = divStep dividend divisor
      newDividendFound = Data.Maybe.isJust $ Data.List.find (\[dividend,quotient]->dividend == newDividend)  divSteps
  in
   if newDividendFound
   then [newDividend,quotient]:divSteps
   else extendDivStepUntilLoop newDividend divisor ([newDividend,quotient]:divSteps)

unitFractionLoopLength n =
  let unitFractionData = extendDivStepUntilLoop 1 n []
  in Data.Maybe.fromJust $ Data.List.findIndex (\ [rem,div] -> rem == head(head(unitFractionData))) $ tail unitFractionData

problem26N n = head $ Data.List.maximumBy loopLengthCompare unitFractionLoopLengths
  where
    loopLengthCompare [_, loopLength1] [_, loopLength2] = compare loopLength1 loopLength2
    unitFractionLoopLengths = Data.List.map idxAndFractionLoopLenght [1..n]
    idxAndFractionLoopLenght i = [i, unitFractionLoopLength i]
problem26 = problem26N 1000


problem27 = (maximumInfo !! 0)*(maximumInfo !! 1)
  where
    maximumInfo = Data.List.maximumBy consecutivePrimeInfoCompare $ consecutivePrimeInfo
    consecutivePrimeInfoCompare [_,_,l1] [_,_,l2] = compare l1 l2
    consecutivePrimeInfo = Data.List.map numberConsecutivePrimes [(x,y)|x<-[-1000..1000],y<-[-1000..1000]]
    numberConsecutivePrimes (a,b) = [a,b,length $ takeWhile Primes.isPrime $ Data.List.map (makePoly a b) [0..]]
    makePoly a b = (\ n->n*n+a*n+b)

problem28N w = 1 + (div (4*n*(8*n^2+15*n+13)) 6)
               where n = (w-1) `div` 2

problem29N n = length $ Map.keys  $ Map.fromList $ Data.List.map (\x->(x,1)) [a^b|a<-[2..n],b<-[2..n]]
problem29 = problem29N 100

problem30 = sum [x|x<-[2..(9^5)*6],x == power5Sum x]
  where power5Sum x = sum $ map (\ i -> i ^ 5) $ numberToDigits x

