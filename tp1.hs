--1
conjeturaDeCollatz :: Integer -> Integer
conjeturaDeCollatz n 
 |n == 1 = 1
 |esPar n = conjeturaDeCollatz (div n 2)
 |otherwise = conjeturaDeCollatz (3*n +1)

esPar :: Integer -> Bool
esPar n = mod n 2 == 0 

satisfaceCollatz :: Integer -> Integer -> Bool
satisfaceCollatz n m 
 |n == 1 && m >= 1 = True
 |m == 1 = False
 |esPar n = satisfaceCollatz (div n 2) (m - 1)
 |otherwise = satisfaceCollatz (3*n + 1) (m - 1)
--2
satisfaceCollatzHastaDesde :: Integer -> Integer -> Integer -> Bool
satisfaceCollatzHastaDesde n k m 
 |k == n && satisfaceCollatz n m  = True
 |satisfaceCollatz k m = satisfaceCollatzHastaDesde n (k+1) m
 |otherwise = False

satisfaceCollatzHasta :: Integer -> Integer -> Bool
satisfaceCollatzHasta n m = satisfaceCollatzHastaDesde n 1 m

--3
cantidadTerminosPares :: Integer -> Integer
cantidadTerminosPares n 
 |n == 1 = 0
 |esPar n = 1 + cantidadTerminosPares (div n 2)
 |otherwise = cantidadTerminosPares (3*n+1)  

--4
largoSecuencia :: Integer -> Integer
largoSecuencia n
 |n == 1 = 0
 |esPar n = 1 + largoSecuencia (div n 2) 
 |otherwise = 1 + largoSecuencia (3*n + 1)

--5
secuenciaMasLargaHastaDesde :: Integer -> Integer -> Integer -> Integer
secuenciaMasLargaHastaDesde n k p
 |k == n && largoSecuencia k > largoSecuencia p = k
 |k == n && largoSecuencia k <= largoSecuencia p = p
 |largoSecuencia k > largoSecuencia p = secuenciaMasLargaHastaDesde n (k+1) k
 |otherwise = secuenciaMasLargaHastaDesde n (k+1) p
 
{- Planteamos una función auxiliar para poder resolver el problema. La función utiliza tres parámetros: un n que funciona como un “hasta” y es el número que se quiere testear, un k que funciona como un “desde” (y va a empezar siendo 1 cuando llamemos a la función), y un p que va a encargarse de arrastrar el número que mayor secuencia tenga. 
Teniendo en cuenta que k va a empezar siendo 1, la función compara todos los valores desde 1 a n, ya que es recursiva con el parámetro k aumentado en una unidad, y se frena cuando k llega es n y compara por última vez.
El parámetro p es muy importante para esta función ya que es una forma de almacenar el k con secuencia más larga hasta el momento para depués compararlo con el siguiente k. 
La función devuelve el mínimo valor m ya que al comparar las secuencias de k y p, si son igual de largas, la función se queda con p. P es menor igual a k. -}
 
secuenciaMasLargaHasta :: Integer -> Integer
secuenciaMasLargaHasta n = secuenciaMasLargaHastaDesde n 1 1

{-Finalizamos el ejercicio llamando a la función auxiliar con k y p con el valor 1. Definimos k = 1 para que empiece a comparar desde 1. Tomamos p = 1 porque necesitamos un número natural lo más chico posible para que sea reemplazado rapidamente por un k con mayor cantidad de pasos.-}
