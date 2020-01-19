data Tree p = Null | Node p (Tree p) (Tree p)
			deriving (Eq, Show, Read)

addNode :: Ord a => a -> Tree a -> Tree a

addNode newitem Null =  Node newitem Null Null --Empty tree base case
addNode newitem (Node rooter left right)
	|newitem < rooter = Node rooter (addNode newitem left) right
	|otherwise = Node rooter left (addNode newitem right)

makeTree :: Ord a => [a] -> Tree a

makeTree [] = Null
makeTree [x] = Node x (Null) (Null)
makeTree (x : xs) = addNode x (makeTree xs)

inOrder :: Tree a -> [a]

inOrder Null = []
inOrder (Node n left right) = inOrder left ++ [n] ++ inOrder right