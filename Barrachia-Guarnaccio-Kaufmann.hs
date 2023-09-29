-- Primer trabajo práctico: La conjetura de Lothar Collatz

-- Ejercicio 1

esPar :: Integer -> Bool
esPar n = n `mod` 2 == 0

{- Definimos esta función para no cargar de cuentas el algoritmo de satisfaceCollatz al momento de que el mismo evalúe la paridad de n. -}

satisfaceCollatz :: Integer -> Integer -> Bool
satisfaceCollatz n m |n == 1 && m >= 1 = True
                     |m == 1 = False
                     |esPar n = satisfaceCollatz (n `div` 2) (m - 1)
                     |otherwise = satisfaceCollatz (3*n + 1) (m - 1)

{- A la hora de definir esta función tuvimos en cuenta el enunciado, el cual no asegura si n satisface la conjetura de Collatz. Con esto en mente,
planteamos que dado un n y una m cantidad de pasos, el algoritmo se fije si n es par o impar y en base a eso haga una recursión siguiendo lo establecido
en la conjetura de Collatz. Cuando hace esta recursión, el programa también resta 1 a m. El propósito de esta resta es fijarnos si con cada recursión n llega a
1 antes de los m pasos. Como los pasos m se van restando de 1 en 1, sabremos que si estos llegan a 1 antes de que n llegue a 1 entonces ese número no cumple
la conjetura para una cantidad menor a los m pasos. Es importante aclarar que consideramos el 1 como caso base para que el algoritmo se frene ya que nos piden que m sea un número natural. -}

-- Ejercicio 2

satisfaceCollatzHastaDesde :: Integer -> Integer -> Integer -> Bool
satisfaceCollatzHastaDesde n k m |k == n && satisfaceCollatz n m = True
                                 |satisfaceCollatz k m = satisfaceCollatzHastaDesde n (k+1) m
                                 |otherwise = False

{- Definimos esta función para resolver un problema más general que facilite la resolución del planteado por el enunciado original. Para ello pensamos en una
función que se encarga de tomar un “hasta” que es n y un “desde” que es k. Así, la función chequea si k satisface Collatz en menos de m pasos para todos los k
entre 1 y n. Para esto se utiliza la recursión, aumentando a k en una unidad cada vez para pasar por todos los números entre 1 y n. La función se frena cuando
k es n y se cumple la conjetura en menos de m pasos (en ese caso el resultado es True) o cuando algún valor entre 1 y n inclusive no cumple la conjetura en menos de m pasos. -}

satisfaceCollatzHasta :: Integer -> Integer -> Bool
satisfaceCollatzHasta n m = satisfaceCollatzHastaDesde n 1 m

{- Finalizamos el ejercicio llamando a la función auxiliar con k = 1 para que empiece a comparar desde 1. Así, constatará si de 1 a n se cumple la conjetura
 de Collatz en menos de m pasos o iteraciones. -}

 -- Ejercicio 3

cantidadTerminosPares :: Integer -> Integer
cantidadTerminosPares n |n == 1 = 0
                        |esPar n = 1 + cantidadTerminosPares (n `div` 2)
                        |otherwise = cantidadTerminosPares (3*n+1)

 {- Esta función se encarga de contar la cantidad de términos de la secuencia de Collatz de un número n que satisface la conjetura,
a partir de a1 = n. Entonces, dado un n, primero analiza si n == 1; y en el caso de que esto pase, la función devuelve 0 ya que 1 es impar.
En el caso de que n sea distinto de 1, analiza si n es par, si lo es suma 1 y repite la función con ese número par dividido 2, para así poder analizar el
término que sigue. En el caso de no ser par no se suma nada y repite la función haciendo la operación 3*n+1, para analizar el término siguiente.
Esto crea un contador que va sumando 1 cada vez que se encuentra con un número par. El contador termina cuando n llega a 1 y esto siempre sucede ya que
tomamos un n que satisface la conjetura de Collatz. -}


-- Ejercicio 4

largoSecuencia :: Integer -> Integer
largoSecuencia n |n == 1 = 0
                 |esPar n = 1 + largoSecuencia (n `div` 2)
                 |otherwise = 1 + largoSecuencia (3*n + 1)

{- Esta función se encarga de contar la cantidad de pasos que realiza un número n que cumple la secuencia de Collatz hasta llegar a 1.
Lo que hace es analizar la paridad o no del número y si este es distinto de 1. En base a eso o se detiene la función (cuando n es 1) o se define una recursión
que depende de la paridad o no de n. En el caso de que se lleve adelante la recursión suma 1 garantizando que se cuenten todos los pasos de la secuencia. -}


-- Ejercicio 5

secuenciaMasLargaHastaDesde :: Integer -> Integer -> Integer -> Integer
secuenciaMasLargaHastaDesde n k p |k == n && largoSecuencia k > largoSecuencia p = k
                                  |k == n && largoSecuencia k <= largoSecuencia p = p
                                  |largoSecuencia k > largoSecuencia p = secuenciaMasLargaHastaDesde n (k+1) k
                                  |otherwise = secuenciaMasLargaHastaDesde n (k+1) p

{- Planteamos una función auxiliar para poder resolver el problema. La función utiliza tres parámetros: un n que funciona como un “hasta” y es el número que se quiere testear,
un k que funciona como un “desde” (va a empezar siendo 1 cuando llamemos a la función), y un p que va a encargarse de arrastrar el número que mayor secuencia tenga.
El parámetro p es muy importante para esta función ya que es una forma de almacenar el k con secuencia más larga hasta el momento para después compararlo con el siguiente k.
Teniendo en cuenta que k va a empezar siendo 1, la función compara todos los valores desde 1 a n, ya que en su recursión k aumenta en una unidad, y se frena cuando k es n
comparándolos por última vez. En esta última comparación, la función devuelve el mínimo valor m  que resulta de comparar las secuencias de k y p, si son igual de largas,
la función se queda con p ya que P es menor igual a k. -}

secuenciaMasLargaHasta :: Integer -> Integer
secuenciaMasLargaHasta n = secuenciaMasLargaHastaDesde n 1 1

{- Finalizamos el ejercicio llamando a la función auxiliar con k y p con el valor 1. Definimos k = 1 para que empiece a comparar desde 1.
Tomamos p = 1 porque necesitamos un número natural lo más chico posible para que sea reemplazado rápidamente por un k con mayor cantidad de pasos. -}
