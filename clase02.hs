-- restricciones sobre variables de tipos
triple :: (Num t) => t -> t
triple x = 3* x

maximo :: (Ord t) => t -> t -> t
maximo x y | x >= y = x
           | otherwise = y

distintos :: (Eq t) => t -> t -> Bool           
distintos x y = x /= y

f1 x y z = x ** y + z <= x + y ** z
{- f1 :: (Ord a, Floating a) => a -> a -> a -> Bool
Ord por <=, Floating por ** -}

f2 x y = ( sqrt x ) / ( sqrt y )
{- f2 :: Floating a => a -> a -> a
Floating por sqrt -}

f3 x y = div ( sqrt x ) ( sqrt y )
{- f3 ::  (Integral a, Floating a) => a -> a -> a
Integral por div, Floating por sqrt
no poner sqrt (Float) con div (Int) -}

f4 x y z | x == y = z
         | x ** y == y = x
         | otherwise = y
-- f4 :: (Eq p, Floating p) => p -> p -> p -> p

f5 x y z | x == y = z
         | x ** y == y = z
         | otherwise = z
-- f5 :: (Eq a, Floating a) => a -> a -> p -> p

-- Suma de vectores en R**2
suma :: ( Float , Float ) -> ( Float , Float ) -> ( Float , Float )
suma v w = (( fst v ) + ( fst w ) , ( snd v ) + ( snd w ))

suma2 :: ( Float , Float ) -> ( Float , Float ) -> ( Float , Float )
suma2 ( vx , vy ) ( wx , wy ) = ( vx + wx , vy + wy )

tercero :: (a, b, c) -> c
tercero (x, y, z) = z

-- Pattern matching sobre tuplas
esOrigen :: ( Float , Float ) -> Bool
esOrigen (0 , 0) = True
esOrigen (_ , _ ) = False

angulo0 :: (Float, Float) -> Bool
angulo0 (_, 0) = True
angulo0 (_,_) = False

angulo45 :: (Float, Float) -> Bool
angulo45 (x, y) = x == y

patternMatching :: (Float, (Bool, Int), (Bool, (Int, Float))) -> (Float, ( Int, Float))
patternMatching (f1, (True, _), (_, (0, f2))) = ( f1 , (1 , f2 ) )
-- p at te rn M at ch i ng ( _ , _ , (_ , (_ , f ) ) ) = (f , (0 , f ) )

-- Parámetros vs. tuplas
normaVectorial2 :: Float -> Float -> Float
normaVectorial2 x y = sqrt (x^2 + y^2)

normaVectorial1 :: (Float , Float) -> Float
normaVectorial1 (x, y) = sqrt (x^2 + y^2)

norma1Suma :: (Float , Float) -> (Float , Float) -> Float
norma1Suma v1 v2 = normaVectorial1 (suma v1 v2)

norma2Suma :: (Float , Float) -> (Float , Float) -> Float
norma2Suma v1 v2 = normaVectorial2 (fst s) (snd s)
         where s = suma v1 v2

{- Funciones binarias: notaci ́on prefija vs. infija
Notación prefija: función antes de los argumentos
Notación infija: función entre argumentos -}

-- Ejercicios
-- 1
estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y = (x <= 3 && y <= 3) || ((x > 3 && x <= 7) && (y > 3 && y <= 7)) || (x > 7 && y > 7) 

--2
prodInt :: ( Float , Float ) -> ( Float , Float ) -> Float
prodInt (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

--3
todoMenor :: ( Float , Float ) -> ( Float , Float ) -> Bool
todoMenor (x1, y1) (x2, y2) = x1 < x2 && y1 < y2

--4
distanciaPuntos ::  ( Float , Float ) -> ( Float , Float ) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt((x1-x2)**2 + (y1-y2)**2)

--5
sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x, y, z) = x+y+z

--6
posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x, y, z) |mod x 2 == 0 = 1
                         |mod y 2 == 0 = 2
                         |mod z 2 == 0 = 3
                         |otherwise = 4
                         
--7
crearPar :: a -> b -> (a,b)
crearPar x y = (x, y)

--8
invertir :: (a, b) -> (b, a)
invertir (x, y) = (y,x)
