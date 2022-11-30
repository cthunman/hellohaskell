
data Tree a where
  Node :: a -> (Tree a) -> (Tree a) -> Tree a
  Empty :: Tree a
  deriving (Show)

leftNode :: Tree a -> Tree a
leftNode (Node _ left _) = left
rightNode :: Tree a -> Tree a
rightNode (Node _ _ right) = right

treeDepth :: Tree a -> Integer
treeDepth Empty = 0
treeDepth t = max (1 + treeDepth (leftNode t)) (1 + treeDepth (rightNode t))
