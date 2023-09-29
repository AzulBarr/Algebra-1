--Listas

esPar :: Int -> Bool
esPar n 
 |n == 0 = True
 |n == 1 = False
 |otherwise = esPar (n-2)

esDivisor :: Int -> Int -> Bool
esDivisor d n = mod n d == 0

{-head [(1,2), (3,4), (5,2)] devuelve el primero
tail [1,2,3,4,4,3,2,1] devuelve todos menos el primero
1 : [6, 5] lo agrega adelante de la lista [1,2] : [] = [[1,2]]
head [1,2,3] : [4,5] = [1,4,5] primero hace head y después lo agrega a la lista
[1,2,3] : [4,5] : [] = [[1,2,3],[4,5]] opera de derecha a izquierda -}

--Recursión sobre listas
sumatoria :: [Int] -> Int
sumatoria l
 |l == [] = 0
 |otherwise = head l + sumatoria (tail l)

longitud :: [Int] -> Int
longitud l
 |l == [] = 0
 |otherwise = 1 + longitud (tail l)

pertenece :: Int -> [Int] -> Bool
pertenece x l
 |l == [] = False
 |x == (head l) = True
 |otherwise = pertenece x (tail l)

primerMultiploDe45345 :: [Int] -> Int
primerMultiploDe45345 l
 |l == [] = undefined
 |esDivisor 45345 (head l) = head l
 |otherwise = primerMultiploDe45345 (tail l)  

--pattern matching
sumatoria2 :: [Int] -> Int
sumatoria2 [] = 0
sumatoria2 (x:xs) = sumatoria2 xs + x

longitud2 :: [ a ] -> Int
longitud2 [] = 0
longitud2 (x:xs) = 1 + longitud2 xs

pertenece2 n [] = False
pertenece2 n (x:xs)
 |n == x = True
 |otherwise = pertenece2 n xs

--Ejercicios
productoria :: [Int] -> Int
productoria l
 |l == [] = 1
 |otherwise = (head l) * productoria (tail l)

productoria2 [] = 1
productoria2 (x:xs) = x * productoria2 xs

sumarN :: Int -> [Int] -> [Int]
sumarN n xs
 |xs == [] = []
 |otherwise = ((head xs) + n) : sumarN n (tail xs)

sumarN2 n [] = []
sumarN2 n (x:xs) = x + n : sumarN2 n xs

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero xs = sumarN (head xs) xs

ultimo (x:[]) = x
ultimo (x:xs) = ultimo xs

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo xs = sumarN (ultimo xs) xs

pares :: [Int] -> [Int]
pares l
 |l == [] = []
 |esPar (head l) = head l : pares (tail l)
 |otherwise = pares (tail l)

multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN n xs
 |xs == [] = []
 |esDivisor n (head xs) = head xs : multiplosDeN n (tail xs)
 |otherwise = multiplosDeN n (tail xs)

quitar :: Int -> [Int] -> [Int]
quitar n l
 |l == [] = []
 |n == head l = tail l
 |otherwise = head l : quitar n (tail l)

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas n l 
 |l == [] = []
 |n == head l = quitarTodas n (tail l)
 |otherwise = head l : quitarTodas n (tail l) 
 
hayRepetidos :: [Int] -> Bool
hayRepetidos l
 |l == [] = False
 |pertenece (head l) (tail l) = True
 |otherwise = hayRepetidos (tail l)


eliminarRepetidos :: [Int] -> [Int]
eliminarRepetidos l
 |l == [] = []
 |pertenece (head l) (tail l) = eliminarRepetidos (head l : quitarTodas (head l) l)
 |otherwise = head l : eliminarRepetidos (tail l)

maximo :: [Int] -> Int
maximo l
 |longitud l == 1 = head l
 |head l >= maximo (tail l) = head l
 |otherwise = maximo (tail l) 

minimo :: [Int] -> Int
minimo l
 |longitud l == 1 = head l
 |head l <= minimo (tail l) = head l
 |otherwise = minimo (tail l)

ordenar :: [Int] -> [Int] --menor a mayor
ordenar l
 |l == [] = l
 |otherwise = minimo l : ordenar (quitar (minimo l) l)
