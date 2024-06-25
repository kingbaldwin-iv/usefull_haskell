type DList a = [a] -> [a]

empty :: DList a
empty = \xs -> xs -- returns a lambda that returns the lists itself

sngl :: a -> DList a
sngl x = \xs -> x : xs -- returns a lambda that appends x to the supplied list xs to the lambda

app :: DList a -> DList a -> DList a
ys `app` zs = \xs -> ys (zs xs) -- returns a lambda that gets a list and supply it to lambda that gets a list which then supply it to another lambda that gets a list 

fromList :: [a] -> DList a
fromList ys = \xs -> ys ++ xs -- returns a lambda that gets a list and append it to the list that was previously supplied

toList :: DList a -> [a]
toList ys = ys [] -- applies the series of lambdas to empty list

type DLog = DList (Integer,Integer)

gcdLog :: Integer -> Integer -> DLog -> (Integer, DLog)
gcdLog a b dlog
 | a == b = (a,log')
 | a > b = gcdLog (a-b) b log'
 | otherwise = gcdLog a (b-a) log'
 where
  log' = dlog `app` sngl (a,b)

gcdGetLog :: Integer -> Integer -> [(Integer,Integer)]
gcdGetLog a b = toList (snd (gcdLog a b empty))


-- BETTER

emptyB :: DList a 
emptyB = id

snglB :: a -> DList a
snglB = (:)

appB :: DList a -> DList a -> DList a
appB = (.)

fromListB :: [a] -> DList a
fromListB = (++)

toListB :: DList a -> [a]
toListB = ($[]) 

gcdLogB :: Integer -> Integer -> DLog -> (Integer, DLog)
gcdLogB a b dlog
 | a == b = (a,log')
 | a > b = gcdLog (a-b) b log'
 | otherwise = gcdLog a (b-a) log'
 where
  log' = dlog `appB` snglB (a,b)
  
gcdGetLogB :: Integer -> Integer -> [(Integer,Integer)]
gcdGetLogB a b = toList (snd (gcdLog a b emptyB))
