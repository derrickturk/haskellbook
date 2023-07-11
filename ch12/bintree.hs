module BinTree where

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Leaf = Node Leaf x Leaf
insert x (Node lhs y rhs)
  | x < y = Node (insert x lhs) y rhs
  | otherwise = Node lhs y (insert x rhs)

contains :: (Ord a) => a -> BinaryTree a -> Bool
contains _ Leaf = False
contains x (Node lhs y rhs)
  | x == y = True
  | x < y = contains x lhs
  | otherwise = contains x rhs

-- really a functor instance
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node lhs x rhs) = Node (mapTree f lhs) (f x) (mapTree f rhs)

-- i.e.
instance Functor BinaryTree
  where fmap = mapTree

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node lhs x rhs) = x:(preorder lhs) ++ preorder rhs

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node lhs x rhs) = preorder lhs ++ [x] ++ preorder rhs

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node lhs x rhs) = preorder lhs ++ preorder rhs ++ [x]

-- inorder fold
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ r Leaf = r
foldTree f r (Node lhs x rhs) = foldTree f (f x (foldTree f r lhs)) rhs

-- I could write this with "maybe" but ehh
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
  Nothing -> Leaf
  Just (l, x, r) -> Node (unfold f l) x (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0 where
  f k
    | k == n = Nothing
    | otherwise = Just (k + 1, k, k + 1)
