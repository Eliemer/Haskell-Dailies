-- module Tree where
-- data Tree a = BinaryTree a | ArbitraryTree a deriving (Eq, Ord, Show, Read)
-- data ArbitraryTree a = Nil | ArbitraryNode a [ (ArbitraryTree a) ]
data BinaryTree a = Nil | BinaryNode a (BinaryTree a) (BinaryTree a) deriving (Eq, Ord, Show, Read)

instance Functor BinaryTree where
  fmap f Nil = Nil
  fmap f (BinaryNode d l r) = BinaryNode (f d) (fmap f l) (fmap f r)

instance Foldable BinaryTree where
  foldMap f Nil = mempty
  foldMap f (BinaryNode d l r) = foldMap f l `mappend` f d `mappend` foldMap f r 

instance Traversable BinaryTree where
  traverse f Nil = pure Nil
  traverse f (BinaryNode d l r) = BinaryNode <$> f d <*> traverse f l <*> traverse f r 

inOrder :: BinaryTree a -> [a]
inOrder Nil = []
inOrder (BinaryNode x left right) = inOrder left ++ [x] ++ inOrder right

preOrder :: BinaryTree a -> [a]
preOrder Nil = []
preOrder (BinaryNode x left right) = x : preOrder left ++ preOrder right

postOrder :: BinaryTree a -> [a]
postOrder Nil = []
postOrder (BinaryNode x left right) = postOrder left ++ postOrder right ++ [x]

