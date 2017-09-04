data Heap a = E | T a [Heap a] deriving Show

findMin :: (Ord a) => Heap a -> a
findMin (T x _) = x

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(T x hs1) h2@(T y hs2)
  | x <= y = T x ([h2] ++ hs1)
  | otherwise = T y ([h1] ++ hs2)

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (T x []) h

mergePaires :: (Ord a) => [Heap a] -> Heap a
mergePaires [] = E
mergePaires [h] = h
mergePaires (h1:h2:hs) = merge (merge h1 h2) (mergePaires hs)

deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin (T x hs) = mergePaires hs

main = do
  let h1 = (T 5 [(T 6 [(T 8 []), (T 7 [])])])
  --let h1 = (T 5 [])

  print $ findMin h1
  print $ insert 3 h1
  print $ deleteMin h1
  print $ deleteMin $ deleteMin h1

