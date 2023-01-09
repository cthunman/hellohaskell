greaterThan100 :: (Ord a, Num a) => [a] -> [a]
greaterThan100 xs = filter (>100) xs

myTest :: [Integer] -> Bool
myTest = even . length . greaterThan100

fold :: t1 -> (t2 -> t1 -> t1) -> [t2] -> t1
fold z f []     = z
fold z f (x:xs) = f x (fold z f xs)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

cartProdThree :: [a] -> [b] -> [c] -> [(a, b, c)]
cartProdThree xs ys zs = [(x, y, z) | x <- xs, y <- ys, z <- zs]

sieveSum :: Num a => a -> a -> a
sieveSum i j = i + j + (2 * i * j)