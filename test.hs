isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

isPrime :: Integer -> Bool
isPrime k = (k > 1) && null [ x | x <- [2..isqrt k], k `mod` x == 0]

nthPrime :: Int -> Integer
nthPrime n = last (take n [x | x <- [1..], isPrime x])

readInt :: String -> Integer
readInt x = read x :: Integer

charToString :: Char -> String
charToString c = [c]

numSlice :: [a] -> Int -> Int -> [a]
numSlice s start len = [s !! x | x <- [start..(start + len - 1)]]

convertToListOfInteger :: [Char] -> [Integer]
convertToListOfInteger s = [readInt (x:"") | x <- s]

calculateProductAtIndex :: Num a => [a] -> Int -> Int -> a
calculateProductAtIndex num_list index len = product (numSlice num_list index len)

largeNumber :: [Char]
largeNumber = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

listOfInteger :: [Integer]
listOfInteger = convertToListOfInteger largeNumber

run :: Int -> Integer
run x = calculateProductAtIndex listOfInteger x 12

-- [run x | x <- [0..999-12], run x /= 0]

-- Here's a problem that combines tuples and list comprehensions: 
-- which right triangle that has integers for all sides and all sides 
-- equal to or smaller than 10 has a perimeter of 24? First, let's try 
-- generating all triangles with sides equal to or smaller than 10:

rightTriangles :: (Num c, Eq c, Enum c) => c -> c -> [(c, c, c)]
rightTriangles maxSideLength targetSum = [(a,b,c) | c <- [1..maxSideLength], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == targetSum]
