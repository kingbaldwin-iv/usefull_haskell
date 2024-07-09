module PropLogic where

-- define the data type Prop a here
-- data Prop a = ...
data Prop a = Var a | Not (Prop a) | And (Prop a) (Prop a) | Or (Prop a) (Prop a)

foldProp :: (b -> c) -> (c -> c) -> (c -> c -> c) -> (c -> c -> c) -> Prop b -> c
foldProp fv fn fa fo (Var x) = fv x
foldProp fv fn fa fo (Not x) = fn (foldProp fv fn fa fo x)
foldProp fv fn fa fo (And x y) = fa (foldProp fv fn fa fo x) (foldProp fv fn fa fo y)
foldProp fv fn fa fo (Or x y) = fo (foldProp fv fn fa fo x) (foldProp fv fn fa fo y)

evalProp :: (a -> Bool) -> Prop a -> Bool
evalProp f = foldProp f not (&&) (||) 

propVars :: Eq a => Prop a -> [a]
propVars = foldl (\x n -> if elem n x then x else n : x) [] . foldProp (\x -> [x]) id (++) (++) 


satProp :: Eq a => Prop a -> Bool
satProp prop = evil3 (run (propVars prop)) prop
 where
  run :: Eq a => [a] -> [[(a,Bool)]]
  run n = [ zip n x | x <- sub (length n)]
   where 
    sub :: Int -> [[Bool]]
    sub 0 = []
    sub 1 = [[True],[False]]
    sub n = [True : x | x <- sub (n-1)] ++ [False : x | x <- sub (n-1)]
  evil3 :: Eq a => [[(a,Bool)]] -> Prop a -> Bool
  evil3 [] _ = False
  evil3 (x:xs) pr 
   | ep x pr = True
   | otherwise = evil3 xs pr
   where 
    ep :: Eq a => [(a,Bool)] -> Prop a -> Bool
    ep f = foldProp (\x -> match f x) not (&&) (||)
     where
      match :: Eq a => [(a,Bool)] -> a -> Bool
      match (a1:as) x
       | x == (fst a1) = snd a1
       | otherwise = match as x
  propVars :: Eq a => Prop a -> [a]
  propVars = foldl (\x n -> if elem n x then x else n : x) [] . foldProp (\x -> [x]) id (++) (++)

instance Show a => Show (Prop a) where
  show = foldProp (\x -> show x) (\x -> "(Not " ++ x ++ ")") (\t -> curry (\x -> "(" ++ fst x ++ " && " ++ snd x ++ ")") t) (\t -> curry (\x -> "(" ++ fst x ++ " || " ++ snd x ++ ")") t) 
