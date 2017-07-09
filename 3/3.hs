
-- メモ
-- Lefist Heap : 最小の者を常に先頭にできるデータ構造
-- 3.1
-- 自分より多きいものを子供に置く。枝が小さい方における。
-- 枝が総量の半分より大きいのは自動的に左に置かれ、配置時に対象にならない

data Heap a =  E | T Int a (Heap a) (Heap a) deriving (Show)

rank E = 0
rank (T r _ _ _) = r

--makeT :: (Ord a) => a -> Heap a -> Heap a -> Heap a
makeT x a1 b1
  | ra >= rb = T (rb + 1) x a1 b1
  | otherwise = T (ra + 1) x b1 a1
  where ra = rank a1
        rb = rank b1

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge E h = h
merge h E = h
merge lh@(T _ x a1 b1) rh@(T _ y a2 b2)
  | x <= y = makeT x a1 (merge b1 rh)
  | otherwise = makeT y a2 (merge b2 lh)

insert x heap = merge heap (T 1 x E E)

-- 3.2
insert' :: (Ord a) => a -> Heap a -> Heap a
insert' x E = T 1 x E E
insert' x h@(T r y a b)
  | x <= y = T 1 x h E
  | otherwise = T r y a (insert x b)

-- 3.3
heapList :: (Ord a) => [a] -> [Heap a]
heapList l = map (\x -> (T 1 x E E)) l

fromList :: (Ord a) => [a] -> Heap a
fromList l = head (fromList' (heapList l))
  where
    fromList' (x:y:z) = fromList' ([merge x y] ++ z)
    fromList' [x] = [x]
    fromList' [] = []


findMin (T _ x _ _) = x
findMin E = error "This heap is empty"

deleteMin (T _ _ a b) = merge a b
deleteMin E = error "This heap is empty"

main = do
  let heap_a = merge (merge (merge (T 1 3 E E) (T 1 5 E E)) (T 1 8 E E)) (T 1 6 E E)
  let heap_b = merge (merge (merge (merge (merge (T 1 2 E E) (T 1 4 E E)) (T 1 7 E E)) (T 1 9 E E)) (T 1 1 E E)) (T 1 5 E E)

  print heap_a
  print heap_b
  print (merge heap_a heap_b)
  print "insert"
  print (insert 10 heap_b)
  print (insert' 10 heap_b)
  print (findMin heap_b)
  print (deleteMin heap_b)

  print "fromList"
  print heap_a
  print (fromList [3, 5, 8, 6])

