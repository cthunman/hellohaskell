addVectors :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- sumThreeVectors :: [t -> (a, b)] -> (a, b) -> (a, b)
-- sumThreeVectors (x1: x2: x3: xr) = addVectors(addVectors(x1 x2) x3)