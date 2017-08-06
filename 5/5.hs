data Q a = BQ [a] [a] deriving (Show)

head' :: Q a -> a
head' (BQ (x:f) r) = x

tail' :: Q a -> Q a
tail' (BQ [x] r) = BQ (reverse r) []
tail' (BQ (x:f) r) = BQ f r

snoc' :: Q a -> a -> Q a
snoc' (BQ [] _) x = BQ [x] []
snoc' (BQ f r) x = BQ f ([x] ++ r)

checkf :: [a] -> [a] -> Q a
checkf [] r = BQ (reverse r) []
checkf f r = BQ f r

tail'' :: Q a -> Q a
tail'' (BQ (x:f) r) = checkf f r

snoc'' :: Q a -> a -> Q a
snoc'' (BQ f r) x = checkf f ([x] ++ r)

main = do
  let q = BQ [1, 2,3] [6, 5, 4]
  let eq = BQ [] []
  let oq = BQ [1] [4, 3, 2]
  print q
  print "head---"
  print (head' q)
  print "tail---"
  print (tail' q)
  print (tail' oq)
  print (tail'' oq)
  print "snoc---"
  print (snoc' q 7)
  print (snoc' eq 7)
  print (snoc'' eq 7)


