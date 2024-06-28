type Vector = [Int]

vecAdd :: Vector -> Vector -> Vector
vecAdd (x:xs) (y:ys) = (x+y) : (vecAdd xs ys)
vecAdd _ _ = []

vecAdd2 :: Vector -> Vector -> Vector
vecAdd2 v1 v2  = map (uncurry (+)) (zip v1 v2)

vecAdd3 :: Vector -> Vector -> Vector
vecAdd3 = zipWith (+) 

type Matrix = [Vector]

matAdd :: Matrix -> Matrix -> Matrix
matAdd (m1:m1s) (m2:m2s) = (vecAdd3 m1 m2) : (matAdd m1s m2s)
matAdd _ _ = []

matAdd2 :: Matrix -> Matrix -> Matrix
matAdd2 m1 m2 = map (uncurry vecAdd3) (zip m1 m2)

matAdd3 :: Matrix -> Matrix -> Matrix
matAdd3 = zipWith vecAdd3

vconst :: Int -> Int -> Vector
vconst 0 _ = []
vconst n x = x : vconst (n-1) x

unit :: Int -> Matrix
unit 0 = []
unit n = (1 : vconst (n-1) 0) : map (0:) (unit (n-1))

tr :: Matrix -> Matrix
tr [] = []
tr [v] = map (\x -> [x]) v
tr (v:vs) = zipWith (:) v (tr vs)

vecMul :: Vector -> Vector -> Vector
vecMul = zipWith (*)

skProd :: Vector -> Vector -> Int
skProd v1 v2 = foldr (+) 0 (vecMul v1 v2)

skProd2 :: Vector -> Vector -> Int
skProd2 v1 v2 = sum (vecMul v1 v2)

matVecMul :: Matrix -> Vector -> Vector 
matVecMul m v = map (`skProd2` v) (tr m)

matMul :: Matrix -> Matrix -> Matrix
matMul m1 m2 = map (matVecMul m1) m2


