
-- datatype Tree = E of Tree * Elem * Tree
data Tree a = ET |
              Elem (Tree a) a (Tree a)
     deriving (Show)

member :: (Ord a) => a -> Tree a -> Bool
member _ ET = False
member x t@(Elem l y r)
  | y > x = member x l
  | y < x = member x r
  | otherwise = True

member' :: (Ord a) => a -> Tree a -> Bool
member' _ ET = False
member' x t@(Elem l y r)
  | y > x = memberl x l l
  | x == y = True
  | otherwise = memberr x r r

memberr :: (Ord a) => a -> Tree a -> Tree a -> Bool
memberr x t ET = member' x t
memberr x t (Elem _ y r)
  | x == y = True
  | otherwise = memberr x t r

memberl :: (Ord a) => a -> Tree a -> Tree a -> Bool
memberl x t ET = member' x t
memberl x t (Elem l y _)
  | x == y = True
  | otherwise = memberl x t l

insert :: (Ord a) => a -> Tree a -> Tree a
insert x ET = Elem ET x ET
insert x t@(Elem l y r)
  | y > x = Elem (insert x l) y r
  | y < x = Elem l y (insert x r)
  | otherwise = t

insert' :: (Ord a) => a -> Tree a -> Tree a
insert' x ET = Elem ET x ET
insert' x t@(Elem l y r)
  | y > x = insert x l
  | y < x = insert x r
  | otherwise = t

main = do
  let t = Elem (Elem ET 5 ET ) 10 (Elem ET 15 ET)
  print (member 10 t)
  print (member 5 t)
  print (member 15 t)
  print (member 6 t)

  let tt = insert 6 t
  print "---"
  print (member 6 tt)
  print (member 6 t)
  print (member 5 tt)
  print (member 5 t)

  let ttt = insert' 6 t
  print "---"
  print (member 6 ttt)
  print (member 6 t)
  print (member 5 ttt)
  print (member 5 t)

  let tttt = Elem (Elem (Elem (Elem ET 3 ET) 4 ET) 5 ET) 10 (Elem ET 15 ET)
  print "2.2"
  print (member' 3 tttt)
  print (member' 5 tttt)

