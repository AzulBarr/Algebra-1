--Combinatoria
type Set a = [a]
combinatorio n m
 |m == n || m == 0 = 1
 |m == 1 = n
 |otherwise = combinatorio (n-1) m + combinatorio (n-1) (m-1)

variaciones :: Set Int -> Int -> Set [Int]
variaciones c l
 |l == 0 = [[]]
 |otherwise = agregarTodosaTodos c (variaciones c (l-1))

agregarTodosaTodos :: Set Int -> Set [Int] -> Set [Int]
agregarTodosaTodos c cc
 |c == [] = []
 |otherwise = union (agregarTodosaTodos (tail c) cc) (agregarTodos (head c) cc)

agregarTodos :: Int -> Set [Int] -> Set [Int]
agregarTodos x c
 |c == [] = []
 |otherwise = agregar (x : (head c)) (agregarTodos x (tail c))

union :: (Eq a) => Set a -> Set a -> Set a
union c1 c2
 |c1 == [] = c2
 |elem (head c1) c2 = union (tail c1) c2
 |otherwise = union (tail c1) (head c1 : c2)

agregar n c
 |elem n c = c
 |otherwise = n : c

--Permutaciones
insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn l n i
 |i == 1 = agregar n l
 |otherwise = (head l): (insertarEn (tail l) n (i-1))

permutaciones :: Int -> [[Int]]
permutaciones n
 |n == 1 = [[1]]
 |otherwise = permutacionesAux (permutaciones (n-1)) n

permutacionesAux :: [[Int]] -> Int -> [[Int]]
permutacionesAux c n
 |c == [] = []
 |otherwise = union (insertarEnTodosLosI (head c) n n) (permutacionesAux (tail c) n)

insertarEnTodosLosI :: [Int] -> Int -> Int -> [[Int]]
insertarEnTodosLosI l n i
 |i == 1 = [n : l]
 |otherwise = (insertarEn l n i) : (insertarEnTodosLosI l n (i-1)) 

--Ejercicios
--1
bolitas :: Int -> Int -> Set [Int]
bolitas n k = variaciones [1..k] n

--2
listasOrdenadas :: Int -> Int -> Set [Int]
listasOrdenadas n k 
 |k == 1 = variaciones [1..n] 1
 |otherwise = agregarTodosaTodosOrdenado [1..n] (listasOrdenadas n (k-1))

agregarTodosaTodosOrdenado :: Set Int -> Set [Int] -> Set [Int]
agregarTodosaTodosOrdenado c cc
 |c == [] = []
 |otherwise = union (agregarElementoAdelanteOrdenado (head c) cc) ( agregarTodosaTodosOrdenado (tail c) cc)

agregarElementoAdelanteOrdenado :: Int -> Set [Int] -> Set [Int]
agregarElementoAdelanteOrdenado x cc 
 |cc == [] = []
 |x < head (head cc) = agregar (x : (head cc)) (agregarElementoAdelanteOrdenado x (tail cc))
 |otherwise = agregarElementoAdelanteOrdenado x (tail cc)

subconjuntos :: Set Int -> Int -> Set (Set Int) 
subconjuntos n k
 |k == 0 = [[]]
 |n == [] = []
 |otherwise = union (agregarElementoAdelante (head n) (subconjuntos (tail n) (k-1))) (subconjuntos (tail n) k)

agregarElementoAdelante :: Int -> Set [Int] -> Set [Int]
agregarElementoAdelante x c
 |c == [] = []
 |otherwise = agregar (x : (head c)) (agregarElementoAdelante x (tail c))

conjuntoDesde1HastaN n = [1..n]