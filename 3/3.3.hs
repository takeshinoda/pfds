
data Color = R | B deriving Show
data Tree a = E | T Color (Tree a) a (Tree a) deriving Show

member :: (Ord a) => a -> Tree a -> Bool
member _ E = False
member x t@(T _ a y b)
  | x < y = member x a
  | x > y = member x b
  | otherwise = True

insert :: (Ord a) => a -> Tree a -> Tree a
insert x s = insert' (ins x s)
  where
    insert' (T _ a y b) = T B a y b
    ins x E = T R E x E
    ins x s@(T color a y b)
      | x < y = balance color (ins x a) y b
      | x > y = balance color a y (ins x b)
      | otherwise = s

balance :: (Ord a) => Color -> Tree a -> a -> Tree a -> Tree a
balance B (T R (T R a x b) y c) z d = (T R (T B a x b) y (T B c z d))
balance B (T R a x (T R b y c)) z d = (T R (T B a x b) y (T B c z d))
balance B a x (T R (T R b y c) z d) = (T R (T B a x b) y (T B c z d))
balance B a x (T R b y (T R c z d)) = (T R (T B a x b) y (T B c z d))
balance c a x b = T c a x b

fromOrdList :: (Ord a) => [a] -> Tree a
fromOrdList [] = E
fromOrdList l = fromOrdList' (fromOrdList'' R l)
  where
    col B = R
    col R = B
    fromOrdList' (T _ a y b) = T B a y b
    fromOrdList'' c [x, y] = T c E x (fromOrdList'' (col c) [y])
    fromOrdList'' c [x] = T c E x E
    fromOrdList'' _ [] = E
    fromOrdList'' c l = T c (fromOrdList'' (col c) (take i l)) (l !! i) (fromOrdList'' (col c) (drop (i + 1) l))
      where i = (length l) `div` 2

main = do
  let t = (insert 15 (insert 5 (insert 10 E)))

  print (member 10 t)
  print (member 13 t)
  print "----"
  print t
  print (insert 13 (insert 15 (insert 5 (insert 10 E))))
  print (insert 13 E)
  print "----"
  print (insert 3 (insert 2 (insert 1 E)))
  print (insert 4 (insert 3 (insert 2 (insert 1 E))))
  print (insert 5 (insert 4 (insert 3 (insert 2 (insert 1 E)))))
  print (insert 7 (insert 6 (insert 5 (insert 4 (insert 3 (insert 2 (insert 1 E)))))))


  print "----"
  print (insert 6 (insert 5 (insert 4 (insert 3 (insert 2 (insert 1 E))))))
  print (fromOrdList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
