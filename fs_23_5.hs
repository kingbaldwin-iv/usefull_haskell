import Data.List (nub)

data Stm = Skip | Assign String Aexp | Seq Stm Stm | If Bexp Stm Stm | While Bexp Stm

data Aexp = Add Aexp Aexp | Mul Aexp Aexp | Var String | Num Integer

data Bexp = Eq Aexp Aexp | Le Aexp Aexp

vars_integer :: Aexp -> [String]
vars_integer (Add e1 e2) = vars_integer e1 ++ vars_integer e2
vars_integer (Mul e1 e2) = vars_integer e1 ++ vars_integer e2
vars_integer (Var v) = [v]
vars_integer (Num i) = []

vars_bool :: Bexp -> [String]
vars_bool (Eq e1 e2) = vars_integer e1 ++ vars_integer e2
vars_bool (Le e1 e2) = vars_integer e1 ++ vars_integer e2

vars_statement :: Stm -> [String]
vars_statement Skip = []
vars_statement (Seq s1 s2) = vars_statement s1 ++ vars_statement s2
vars_statement (While e s1) = vars_bool e ++ vars_statement s1
vars_statement (Assign s e) = [s] ++ vars_integer e
vars_statement (If e s1 s2) = vars_bool e ++ vars_statement s1 ++ vars_statement s2

vars :: Stm -> [String]
vars = nub . vars_statement

type State = [(String,Integer)]

empty :: State 
empty = []

(!) :: State -> String -> Integer
(!) ((d,r):xs) a
 | d == a = r
 | otherwise = (!) xs a 

insert :: String -> Integer -> State -> State
insert a b ((d,r):xs) 
 | d == a = (d,b):xs
 | otherwise = (d,r) : insert a b xs
insert a b [] = [(a,b)]

fromList :: [(String, Integer)] -> State
fromList = id

eval_integer :: State -> Aexp -> Integer
eval_integer s (Add e1 e2) = (eval_integer s e1) + (eval_integer s e2)
eval_integer s (Mul e1 e2) = (eval_integer s e1) * (eval_integer s e2) 
eval_integer s (Var v) = s ! v
eval_integer s (Num i) = i

eval_bool :: State -> Bexp -> Bool
eval_bool s (Eq e1 e2) = (eval_integer s e1) == (eval_integer s e2)
eval_bool s (Le e1 e2) = (eval_integer s e1) < (eval_integer s e2)

eval_statement :: State -> Stm -> State
eval_statement s Skip = s
eval_statement s (Assign v e) = insert v (eval_integer s e) s 
eval_statement s (Seq s1 s2) = eval_statement (eval_statement s s1) s2
eval_statement s (If e s1 s2) = if (eval_bool s e) then eval_statement s s1 else eval_statement s s2
eval_statement s (While e s1) = if (eval_bool s e) then eval_statement (eval_statement s s1) (While e s1) else s

insert_all :: String -> Integer -> State -> [State]
insert_all v b s = map (\i -> insert v i s) [0..b]

all_states :: [String] -> Integer -> [State]
all_states [] b = [[]]
all_states (h:t) b = concat (map (insert_all h b) (all_states t b))

test :: Integer -> (Bexp, Stm, Bexp) -> Bool
test n (b1,s,b2) = foldl (&&) True (map (\x -> eval_bool x b2) (map (\x -> eval_statement x s) (filter (\x -> eval_bool x b1) (all_states (vars_bool b1) n))))



