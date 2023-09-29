-- 1
abso :: Int -> Int
abso x |x>0 = x
           |x<0 = -x
           |otherwise = 0

-- 2
maxAbso :: Int -> Int -> Int
maxAbso x y |ax>ay = ax
            |ax<ay = ay
            where ax |x>0 = x
                     |x<0 = -x
                  ay |y>0 = y
                     |y<0 = -y

-- 3
max3 :: Int -> Int -> Int -> Int
max3 x y z |x>y && x>z = x
           |y>x && y>z = y
           |otherwise = z

-- 4
algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y |x == 0 || y == 0 = True
              |otherwise = False

-- 5
ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y |x == 0 && y == 0 = True
              |otherwise = False

-- 6
multiplos :: Int -> Int -> Bool 
multiplos x y |mod x y == 0 = True   
              |otherwise = False
-- |x<=0 || y<=0 = Ignore

-- 7
digitoUnidades :: Int -> Int
digitoUnidades x = x - div x 10 * 10

-- 8
digitoDecenas :: Int -> Int
digitoDecenas x = x - div x 100 * 100 - (x - div x 10 * 10)