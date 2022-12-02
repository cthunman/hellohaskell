import Data.List
import Distribution.Simple.Utils (xargs)

myDrop :: (Ord t, Num t) => t -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

isOdd :: Integral a => a -> Bool
isOdd n = mod n 2 == 1

lastButOne :: [a] -> a
lastButOne xs = if length xs < 2
    then error "Not enough elements in the list."
    else if length xs == 2 then head xs
    else lastButOne (tail xs)


tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _       = Nothing

tidyLastButOne :: [a] -> Maybe a
tidyLastButOne [] = Nothing
tidyLastButOne (x:[]) = Nothing
tidyLastButOne (x:_:[]) = Just x
tidyLastButOne (x:xs) = tidyLastButOne xs
 
data BookInfo = Book Integer String [String]
    deriving (Show)

data MagazineInfo = Magazine Integer String [String]
    deriving (Show)

type CustomerID = Integer
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID ReviewBody

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

data List a = Cons a (List a)
            | Nil
              deriving (Show)

data Customer = Customer Int String [String]
                deriving (Show)

customerID :: Customer -> Int
customerID (Customer id _ _) = id

customerName :: Customer -> String
customerName (Customer _ name _) = name

customerAddress :: Customer -> [String]
customerAddress (Customer _ _ address) = address


quux :: p -> [Char]
quux a = let a = "foo"
         in a ++ "eek!"

pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"

lend3 :: (Ord a, Fractional a) => a -> a -> Maybe a
lend3 amount balance
     | amount <= 0            = Nothing
     | amount > reserve * 0.5 = Nothing
     | otherwise              = Just newBalance
    where reserve    = 100
          newBalance = balance - amount

lengthOfList :: [a] -> Integer
lengthOfList [] = 0
lengthOfList (x:[]) = 1
lengthOfList (x:xs) = 1 + lengthOfList xs

sumOfList :: [Double] -> Double
sumOfList [] = 0
sumOfList (x:[]) = x
sumOfList (x:xs) = x + sumOfList xs

meanOfList :: [Double] -> Double
meanOfList [] = 0
meanOfList (x:[]) = x
meanOfList xs = (/) (sumOfList xs) (fromIntegral (lengthOfList xs))

createPalindrome :: [a] -> [a]
createPalindrome [] = []
createPalindrome (x:[]) = x:x:[]
createPalindrome (x:xs) = [x] ++ (createPalindrome xs) ++ [x]

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:[]) = [x]
reverseList (x:xs) = (reverseList xs) ++ [x]

testPalindrome :: Eq a => [a] -> Bool
testPalindrome [] = True
testPalindrome (x:[]) = True
testPalindrome (xs) = xs == reverseList xs

intersperseMe :: a -> [[a]] -> [a]
intersperseMe c [] = []
intersperseMe c (x:[]) = x
intersperseMe c (x:xs) = x ++ [c] ++ (intersperseMe c xs)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (p:xs) = (quickSort lesser) ++ [p] ++ (quickSort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] [] = []
mergeLists xs [] = xs
mergeLists [] xs = xs
mergeLists (x:xs) (y:ys) = if x <= y
    then [x] ++ mergeLists xs (y:ys)
    else [y] ++ mergeLists (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort (x:[]) = [x]
mergeSort xs = mergeLists (mergeSort firstHalf) (mergeSort secondHalf)
    where
        firstHalf = fst (splitAt (div (length xs) 2) xs)
        secondHalf = snd (splitAt (div (length xs) 2) xs)
