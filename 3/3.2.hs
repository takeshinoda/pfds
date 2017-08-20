
data Tree a = Node Int a [Tree a] deriving (Show)
type Heap a = [Tree a]

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
  | x1 <= x2 = Node (r + 1) x1 ([t2] ++ c1)
  | otherwise = Node (r + 1) x2 ([t1] ++ c2)

rank (Node r _ _) = r

insert :: (Ord a) => a -> Heap a -> Heap a
insert x ts = insTree (Node 0 x []) ts

insTree :: (Ord a) => Tree a -> [Tree a] -> [Tree a]
insTree t [] = [t]
insTree t ts@(t':ts')
  | rt < rt' = [t] ++ ts
  | otherwise = insTree (link t t') ts'
  where rt  = rank t
        rt' = rank t'

main = do
  let ts = []
  print (insert 4 (insert 2 (insert 5 (insert 3 ts))))
  print (insert 6 (insert 4 (insert 2 (insert 5 (insert 3 ts)))))
  print (insert 7 (insert 6 (insert 4 (insert 2 (insert 5 (insert 3 ts))))))
  print (insert 5 (insert 3 ts))

