data Q a = BQ [a] [a] deriving (Show)

cons' :: Q a -> a -> Q a
cons' (BQ f r) x = checkf ([x] ++ f) r

head' :: Q a -> a
head' (BQ (x:f) r) = x

tail' :: Q a -> Q a
tail' (BQ (x:f) r) = checkf f r

snoc' :: Q a -> a -> Q a
snoc' (BQ f r) x = checkf f ([x] ++ r)

fr :: [a] -> Q a
fr x = BQ ((take (harf x) x) . reverse) (drop (harf x) x)
  where
    harf x = (length x) `div` 2

checkf :: [a] -> [a] -> Q a
checkf [] r = fr r
checkf f [] = fr f
checkf f r = BQ f r

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
  print "snoc---"
  print (snoc' q 7)
  print (snoc' eq 7)


