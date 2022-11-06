toDigits :: Integer -> [Integer]
toDigits n = reverseList (toDigitsRev n)


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | n `div` 10 == 0 = n : []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)


intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs


isIntValidLength :: Integer -> Bool
isIntValidLength n = isValidLength (toDigits n)


isValidLength :: [Integer] -> Bool
isValidLength l = intListLength l == 16


reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) = x : (2*y) : doubleEveryOther zs


doubleEveryOtherFromRight :: [Integer] -> [Integer]
doubleEveryOtherFromRight l = reverseList(doubleEveryOther(reverseList l))


sumListValues :: [Integer] -> Integer
sumListValues [] = 0
sumListValues (x:xs) = x + sumListValues xs


listValuesToDigits :: [Integer] -> [Integer]
listValuesToDigits [] = []
listValuesToDigits (x:xs) = toDigits x ++ listValuesToDigits xs


validationSum :: Integer -> Integer
validationSum n = sumListValues (listValuesToDigits (doubleEveryOther (toDigitsRev n)))


validate :: Integer -> Bool
validate n = ((validationSum n) `mod` 10 == 0) && (isIntValidLength n)
