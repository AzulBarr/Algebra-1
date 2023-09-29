--Sumatorias
--1
s1 :: Integer -> Integer 
s1 n |n == 0 = 1 
     |n > 0 = 2^n + s1 (n-1)

--2
s2 :: Integer -> Float -> Float
s2 n q |n == 1 = q 
       |n > 1 = q^n + s2 (n-1) q

--3
s3 :: Integer -> Float -> Float
s3 n q = s2 (2*n) q

--4
s4 :: Integer -> Float -> Float 
s4 n q = s3 n q - s2 (n-1) q 

--5
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

--6
eAprox :: Integer -> Float
eAprox n |n == 0 = 1
         |n > 0 = 1/(fromIntegral (fact n)) + eAprox (n-1)

e:: Float
e = eAprox 10
  
--Sumatorias dobles

--1

ss1 :: Integer -> Integer -> Integer
ss1 n m |n == 0 = 0
        |n > 0 = round (s2 m (fromIntegral n)) + ss1 (n-1) m

--2
sumaPot q n m |n == 0 = 0 
              |n > 0 = sumaPot q (n-1) m + (q^n) * (s2 m q)

sumaPot2 q n m = (s2 n q) * (s2 m q)

--3
sumaRacioAux n m |m < 1 = 0
                 |otherwise = n/m + sumaRacioAux n (m-1)
                
sumaRacio n m |n < 1 = 0
              |otherwise = sumaRacioAux n m + sumaRacioAux (n-1) m

--4
g1 i n |n < i = 0
       |otherwise = i^n + g1 i (n-1) 

--5
g2 n |n == 0 = 0
     |n > 0 = round (s2 n (fromIntegral n)) + ss1 (n-1) n