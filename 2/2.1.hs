
tail' :: [a] -> [a]
tail' [] = error "This list is empty"
tail' (_:x) = x

suffixes :: [l]-> [[l]]
suffixes [] = [[]]
suffixes l = [l] ++ (suffixes . tail') l
--suffixes l = [l] ++ (suffixes . (\ (_:x) -> x)) l -- ワンライナーで書くにはこれでいい？

main = do
  print (suffixes [1, 2, 3, 4])

