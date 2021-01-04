-- module Tree where
-- data Tree a = BinaryTree a | ArbitraryTree a deriving (Eq, Ord, Show, Read)
-- data ArbitraryTree a = Nil | ArbitraryNode a [ (ArbitraryTree a) ]
data BinaryTree a = Nil | BinaryNode a (BinaryTree a) (BinaryTree a) deriving (Eq, Ord, Show, Read)

-- Example construction
-- Empty Tree
-- Nil

-- -- Root Node with empty children
-- Node "first" []

-- -- Root Node with children
-- Node "root" [Node "left" [], Node "right" []]

sumTree :: Num a => BinaryTree a -> a
sumTree Nil = 0
sumTree (BinaryNode x left right) = x + sumTree left + sumTree right

inOrder :: BinaryTree a -> [a]
inOrder Nil = []
inOrder (BinaryNode x left right) = inOrder left ++ [x] ++ inOrder right

preOrder :: BinaryTree a -> [a]
preOrder Nil = []
preOrder (BinaryNode x left right) = x : preOrder left ++ preOrder right

postOrder :: BinaryTree a -> [a]
postOrder Nil = []
postOrder (BinaryNode x left right) = postOrder left ++ postOrder right ++ [x]

