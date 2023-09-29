--Polinomios
 
type Polinomio = [Float]
type Monomio = (Float, Int)
 
--1
crearPolinomio :: [Float] -> Polinomio
crearPolinomio [] = []
crearPolinomio (x:xs) | x == 0 = crearPolinomio xs
                      | otherwise = (x:xs)
 
-- Esta función es la encargada de hacer que una lista cumpla el invariante del tipo polinomio. Para ello, se va a encargar de una vez dada una lista borrar todos los 0s iniciales. Así, dada una lista se fija si la cabeza de la misma es 0. En el caso de que no sea 0, el algoritmo devuelve la lista original. En caso de que sea 0, se plantea una recursión con la cola, donde ahora se fija si la cabeza de esta nueva lista vuelve a ser 0 o no. Si vuelve a ser 0 se repite el proceso hasta que no haya ningún 0 o hasta que la lista sea la vacía, caso donde la lista no cumple el invariante polinomio.
 
--2
grado :: Polinomio -> Int
grado [] = undefined
grado [_] = 0
grado (x:xs) = 1 + grado xs
 
-- Esta función es la encargada de contar el grado que va a tener el polinomio. Para ello analiza los elementos de la lista del siguiente modo: si el polinomio está formado por la lista vacía entonces respetamos la convención y explicitamos que no está definido el grado; si el polinomio está formado por una lista de un único elemento entonces el grado es cero ya que ese elemento corresponde al término independiente; y, por último, si el polinomio está formado por una lista de más de 1 elemento se plantea una recursión que va a contar el grado del polinomio. La recursión lo que hace es analizar la cantidad de elementos que tiene la lista distintos del término independiente. Entonces, cómo el primer elemento es siempre distinto de 0 ya que respeta el invariante propuesto en la consigna, la función suma 1. Luego se fija si en la cola que queda la lista está formada por 1 elemento o más de uno. Si está formada por 1 elemento vamos al caso base y suma 0. Si está formada por más de 1 elemento vuelve a plantear la recursión.

--3
evaluar :: Polinomio -> Float -> Float
evaluar [] _ = 0
evaluar [b] 0 = b
evaluar (x:xs) a = x * a^(grado (x:xs)) + evaluar xs a
 
--4
longitud :: [Float] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
 
agregarCero :: Polinomio -> Int -> Polinomio
agregarCero [p] 1 = p : [0]
agregarCero (x:xs) i = x : agregarCero xs (i-1)

agregarCerosAPol :: Polinomio -> Float -> Polinomio
agregarCerosAPol p 0 = p
agregarCerosAPol p 1 = agregarCero p (longitud p)
agregarCerosAPol p b = agregarCerosAPol (agregarCerosAPol p 1) (b-1)
 
numeroPorPolinomio :: Polinomio -> Float -> Polinomio
numeroPorPolinomio [] a = []
numeroPorPolinomio (x:xs) a = a * x : numeroPorPolinomio xs a
 
productoPorMonomio :: Monomio -> Polinomio -> Polinomio
productoPorMonomio (a,b) p = agregarCerosAPol (numeroPorPolinomio p a) (fromIntegral b)

--5
suma :: Polinomio -> Polinomio -> Polinomio
suma [] q = q
suma p [] = p
suma (x:xs) (y:ys)
 |grado p < grado q = y : suma p ys
 |grado p > grado q = x : suma xs q
 |otherwise = x+y : suma xs ys
 where p = (x:xs)
       q = (y:ys)
 
producto :: Polinomio -> Polinomio -> Polinomio
producto [] q = []
producto (x:xs) q = suma (productoPorMonomio (x, grado p) q) (producto xs q)
 where p = (x:xs)
 
--6
longitudPar :: [Float] -> Bool
longitudPar l = mod (longitud l) 2 == 0
 
evaluacionMultiple :: [Float] -> Polinomio -> Polinomio -> [Float]
evaluacionMultiple [x] p q = evaluar p x : []
evaluacionMultiple (x:xs) p q
 |longitudPar (x:xs) = evaluar q x : evaluacionMultiple xs p q
 |otherwise = evaluar p x : evaluacionMultiple xs p q
 
{- Esta función es la encargada de, una vez dada una lista conformada por al menos 1 elemento devolver otra lista donde en los lugares pares vamos a encontrar el resultado de evaluar un polinomio P(x) por el real que le correspondía a ese lugar par y en los lugares impares el resultado de evaluar un polinomio Q(x) por el real correspondiente a ese lugar.
Así, el algoritmo va a fijarse la longitud de la lista. Si la lista está formada por un solo elemento entonces vamos a estar hablando de la posición a0. Como 0 es par, va a devolver una lista donde su único elemento va a ser P(a0).
Si la lista tiene una longitud de más de un elemento, se va a fijar si la posición de la cabeza es par o impar y dependiendo de eso va a evaluarla en P(x) o en Q(x) (volviendo a este valor la cabeza de una nueva lista) y, luego, va a plantearse una recursión con la cola de la lista. A la cola la va a considerar una nueva lista y va a hacer todo este procedimiento nuevamente hasta llegar a la posición a0. Veamos que pasa según las longitudes.
Si la lista tiene una longitud par entonces la posición ak corresponde a un lugar impar (estamos contando desde a0 y no a1). Entonces el algoritmo va a hacer Q(ak), lo va a convertir en la cabeza de la lista resultado, y va a plantear una recursión con la cola de la lista.
Si la lista tiene una longitud impar entonces la posición ak corresponde a un lugar par (estamos contando desde a0 y no a1). Entonces el algoritmo va a hacer P(ak), lo va a convertir en la cabeza de la lista resultado, y va a plantear una recursión con la cola de la lista.
-}