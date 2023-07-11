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

-- tests, from the book:
testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node lhs x rhs) = x:(preorder lhs) ++ preorder rhs

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node lhs x rhs) = preorder lhs ++ [x] ++ preorder rhs

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node lhs x rhs) = preorder lhs ++ preorder rhs ++ [x]

-- tests, from book
testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

-- inorder fold
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ r Leaf = r
foldTree f r (Node lhs x rhs) = foldTree f (f x (foldTree f r lhs)) rhs
