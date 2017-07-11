
data Tree a = Node Int a [Tree a] deriving (Show)

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
  | x1 <= x2 = Node (r + 1) x1 ([t2] ++ c1)
  | otherwise = Node (r + 1) x2 ([t1] ++ c2)

main = do
  let t = Node 2 1 [(Node 1 2 [])]
  print t

