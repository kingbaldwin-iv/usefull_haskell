data Literal a = Prop a | NotProp a deriving (Eq,Show)

type Clause a = [Literal a]
type CNF a = [Clause a]

var :: Literal a -> a
var (Prop a) = a
var (NotProp a) = a

neg :: Literal a -> Literal a
neg (Prop a) = NotProp a
neg (NotProp a) = Prop a

taut :: Eq a => Clause a -> Bool
taut a = any (\x -> elem (neg x) a) a

complVar :: Eq a => Clause a -> Clause a -> [a]
complVar a b = map var (filter (\x -> elem (neg x) a) b)

resolve :: Eq a => Clause a -> Clause a -> a -> Clause a
resolve a b c = filter (\x -> (var x) /= c) (a++b)

--step :: Eq a => CNF a -> CNF a

inj :: [a] -> [(a,a)]
inj [] = []
inj (x:xs) = [(x,y) | y <- xs] ++ inj xs

inj2 :: Eq a => CNF a -> [(Clause a, Clause a)]
inj2 a = filter (\x -> taut (uncurry (++) x)) (inj a)

step :: Eq a => CNF a -> CNF a
step a = if (length (inj2 a) > 0) then run (head (inj2 a)) a else a
 where 
  run :: Eq a => (Clause a,Clause a) -> CNF a -> CNF a
  run (e1, e2) a = (remove 1 (e1,e2) a) ++ [resolve e1 e2 (head (complVar e1 e2))]
   where 
    remove :: Eq a => Integer -> (Clause a, Clause a) -> CNF a -> CNF a
    remove n (e1,e2) (x:xs)
     | e1 == x && n == 1 = remove 2 (e1,e2) xs
     | e2 == x && n == 2 = xs
     | otherwise = x : remove n (e1,e2) xs
    remove _ _ [] = []

sat :: Eq a => CNF a -> Bool
sat c = if elem [] r then False else (if r == c then True else sat r) 
 where r = step (filter (not . taut) c)

