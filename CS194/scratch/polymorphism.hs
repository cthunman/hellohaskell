
data List t where
  E :: List t
  C :: t -> (List t) -> List t
  deriving Show

filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C x xs)
  | p x       = C x (filterList p xs)
  | otherwise = filterList p xs

lst1 :: List Integer
lst1 = C 3 (C 5 (C 2 E))

mapList :: (a -> b) -> List a -> List b
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)
