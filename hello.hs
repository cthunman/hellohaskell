
--some project euler problems I'm using to get familiar with haskell

--find sum of all multiples of 5 or 3 less than 1000
let multiples = [x | x <- [1..999], x `mod` 5 == 0 ||  x `mod` 3 == 0]
sum multiples


--find sum of even fibonacci numbers with values less than 4000000
let fibs = 0 : 1 : next fibs where next (a : t@(b:_)) = (a+b) : next t
let fibonacci = take 12 [x | x <- fibs, x < 4000000, x `mod` 2 == 0]
sum fibonacci


