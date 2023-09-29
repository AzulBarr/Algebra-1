esDivisor :: Integer -> Integer -> Bool
esDivisor d n = mod n d == 0
 
mcd :: Integer -> Integer -> Integer
mcd a b
 |b == 0 = a
 |otherwise = mcd b (mod a b) --snd division2 a b
 
division2 :: Integer -> Integer -> (Integer, Integer)
division2 a d
 |a < d = (0, a)
 |otherwise = (q+1, r)
 where (q, r) = division2 (a-d) d
 
emcd :: Integer -> Integer -> (Integer, Integer, Integer) --(mcd, s, t) as+bt
emcd a b
 |b == 0 = (a, 1, 0)
 |otherwise = ((mcd a b), s, t)
 where s = trd3 (emcd b (mod a b))
       t = snd3 (emcd b (mod a b)) - s * fst (division2 a b)

fst3 (x , _ , _ ) = x
snd3 (_ , y , _ ) = y
trd3 (_ , _ , z ) = z
 
coprimizar :: (Integer,Integer,Integer) -> (Integer,Integer,Integer)
coprimizar (a,b,m) |esDivisor d b = (div a d, div b d, div m d)
                   |otherwise = undefined
                   where d = mcd a m
 
solucionEcAux :: (Integer,Integer,Integer) -> (Integer,Integer)
solucionEcAux (a,b,m) = (mod (s*b) m, m)
 where s = snd3 (emcd a m)
 
solucionEc :: (Integer, Integer, Integer) -> (Integer, Integer)
solucionEc (a,b,m) = solucionEcAux (coprimizar (a,b,m))
 
sistemaSimplifEquiv :: [(Integer, Integer, Integer)] -> [(Integer, Integer)]
sistemaSimplifEquiv [] = []
sistemaSimplifEquiv (x:xs) = solucionEc x : sistemaSimplifEquiv xs
 
todosLosPrimosMalos :: [(Integer, Integer)] -> [Integer]
todosLosPrimosMalos [] = []
todosLosPrimosMalos sist = todosLosPrimosMalosHasta sist (maximum (modulos sist))
 
todosLosPrimosMalosHasta :: [(Integer,Integer)] -> Integer -> [Integer]
todosLosPrimosMalosHasta _ 0 = []
todosLosPrimosMalosHasta sist n |esPrimoMalo sist n = n : (todosLosPrimosMalosHasta sist (n-1))
                                |otherwise = todosLosPrimosMalosHasta sist (n-1)
 
esPrimoMalo :: [(Integer, Integer)] -> Integer -> Bool
esPrimoMalo sist n = (esPrimo n) && cantidadMultiplos (modulos sist) n >= 2
 
modulos :: [(Integer, Integer)] -> [Integer]
modulos [] = []
modulos ((r,m): es) = m :(modulos es)
 
cantidadMultiplos :: [Integer] -> Integer -> Integer
cantidadMultiplos [] _ = 0
cantidadMultiplos (m:ms) n |mod m n == 0 = 1 + cantidadMultiplos ms n
                           |otherwise = cantidadMultiplos ms n

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorAux 2 n
 
menorDivisorAux :: Integer -> Integer -> Integer
menorDivisorAux t n |esDivisor t n = t
                    |otherwise = menorDivisorAux (t+1) n
                  
esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n == n
 
solucSistemaPotenciasPrimo :: [(Integer, Integer)] -> (Integer,Integer)
solucSistemaPotenciasPrimo [e] = e
solucSistemaPotenciasPrimo (e1:e2:es) = solucSistemaPotenciasPrimo (solucDosEcPotenciasPrimo e1 e2: es)
 
solucDosEcPotenciasPrimo :: (Integer, Integer) -> (Integer,Integer) -> (Integer,Integer)
solucDosEcPotenciasPrimo (r1,m1) (r2,m2) |m1 <= m2 = solucDosEcPotenciasPrimoOrd (r1,m1) (r2,m2)
                                         |otherwise = solucDosEcPotenciasPrimoOrd (r2,m2) (r1,m1)
 
solucDosEcPotenciasPrimoOrd :: (Integer, Integer) -> (Integer,Integer) -> (Integer,Integer)
solucDosEcPotenciasPrimoOrd (r1,m1) (r2,m2) |esDivisor m1 (r2-r1) = (r2,m2)
                                            |otherwise = undefined
 
desdoblarSistemaEnFcionPrimo :: [(Integer, Integer)] -> Integer -> ([(Integer, Integer)], [(Integer, Integer)])
desdoblarSistemaEnFcionPrimo [] _ = ([], [])
desdoblarSistemaEnFcionPrimo ((r,m):es) p
 |k == 0 = (pri, (r,m):seg)
 |m == p^k = ((r,m):pri, seg)
 |otherwise = ((mod r (p^k), p^k):pri, (mod r (div m (p^k)), div m (p^k)):seg)
 where (pri, seg) = desdoblarSistemaEnFcionPrimo es p
       k = quePotenciaLoDivide m p
 
quePotenciaLoDivide :: Integer -> Integer -> Integer
quePotenciaLoDivide p m |esDivisor p m = 1 + quePotenciaLoDivide p (div m p)
                        |otherwise = 0
 
sistemaEquivSinPrimosMalos :: [(Integer,Integer)] -> [(Integer,Integer)]
sistemaEquivSinPrimosMalos sist = sistemaEquivSinPrimosMalosAux sist (todosLosPrimosMalos sist)
 
sistemaEquivSinPrimosMalosAux :: [(Integer,Integer)] -> [Integer] -> [(Integer,Integer)]
sistemaEquivSinPrimosMalosAux sist [] = sist
sistemaEquivSinPrimosMalosAux sist (p:ps) = (solucSistemaPotenciasPrimo pri) : (sistemaEquivSinPrimosMalosAux seg ps)
 where (pri,seg) = desdoblarSistemaEnFcionPrimo sist p
 
solucSistemaModCoprimos :: [(Integer, Integer)] -> (Integer, Integer)
solucSistemaModCoprimos [e] = e
solucSistemaModCoprimos ((r1,m1) : (r2,m2) : es) = solucSistemaModCoprimos (( r,m1*m2) :es)
 where (d,s,t) = emcd m1 m2
       r = mod (r1*t*m2 + r2*s*m1) (m1*m2)
 
solucSistema :: [(Integer, Integer, Integer)] -> (Integer, Integer)
solucSistema sist = solucSistemaModCoprimos ( sistemaEquivSinPrimosMalos ( sistemaSimplifEquiv sist))
