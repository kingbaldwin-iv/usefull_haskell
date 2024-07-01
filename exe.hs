removeDuplicates :: Eq a => [a] -> [a]

removeDuplicates [] = []
removeDuplicates (x:xs) 
 | count x xs == 0 = x : removeDuplicates xs
 | otherwise = removeDuplicates xs
 where
  count :: Eq a => a -> [a] -> Integer
  count _ [] = 0
  count n (x:xs) 
   | x == n = 1+ count n xs
   | otherwise = count n xs


symmetricDifference :: Eq a => [a] -> [a] -> [a]

symmetricDifference [] a = a
symmetricDifference a [] = a
symmetricDifference (x:xs) (y:ys) 
 | True = (isolate (x:xs) (y:ys)) ++ (isolate (y:ys) (x:xs))
 where 
  isolate :: Eq a => [a] -> [a] -> [a]
  isolate a [] = a
  isolate [] _ = []
  isolate (x:xs) (y:ys)
   | elem2 x (y:ys) = isolate xs (y:ys)
   | otherwise = x : isolate xs (y:ys)
   where 
    elem2 :: Eq a => a -> [a] -> Bool 
    elem2 _ [] = False
    elem2 n (x:xs) 
     | n == x = True
     | otherwise = elem2 n xs


bst :: Int -> Int -> Int
bst x y = min (st x 0) y
 where
  st :: Int -> Int -> Int
  st x y 
   | x == 1 = y
   | mod x 2 == 0 = st (div x 2) (y+1)
   | otherwise = st (((*) x 3)+1) (y+1)

gaps :: [Int] -> [[Int]]
gaps [] = []
gaps (x:xs) = gapss (x:xs) (length (x:xs))
 where 
  gapss :: [Int] -> Int -> [[Int]]
  gapss _ 0 = []  
  gapss (x:xs) n = (getRid n (x:xs)) : (gapss (x:xs) (n-1))
   where 
    getRid :: Int -> [Int] -> [Int]
    getRid n (x:xs) = take (n-1) (x:xs) ++ drop n (x:xs)


digitProd :: Int -> Int -> Int
digitProd k n = evil n 7 1 k
 where 
  evil :: Int -> Int -> Int -> Int -> Int
  evil _ _ _ 0 = 1
  evil n m d k = (div (mod n m) d) * evil n (m*7) m (k-1)

tokenization :: Int -> Int 
tokenization x = read (run (show x)) :: Int
 where 
  run :: String -> String
  run [a] = [a]
  run [a,b] 
   | a == b = [a]
   | otherwise = [a,b]
  run (x:y:ys) 
   | x == y = run (y:ys)
   | otherwise = x : run (y:ys)


triangular :: Int -> Bool 
triangular 1 = True
triangular x = evil x 1 2
 where 
  evil :: Int -> Int -> Int -> Bool
  evil x n d
   | n == x = True
   | n < x = evil x (n+d) (d+1)
   | otherwise = False

