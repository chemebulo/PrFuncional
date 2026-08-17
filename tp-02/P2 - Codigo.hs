--------------------------------------------------------------------------------------------------------

--                               TRABAJO PRÁCTICO N°2: Sistemas de Tipos                              --

--------------------------------------------------------------------------------------------------------

> Ejercicio 1:

-- 1.A

first :: (a, b) -> a
first (x, y) = x 

-- 1.B

apply :: (a -> b) -> (a -> b)
apply f = g
    where g x = f x

-- 1.C

twice :: (a -> a) -> (a -> a)
twice f = g
    where g x = f (f x)

-- 1.D

doble :: Int -> Int
doble x = x + x

-- 1.E

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- 1.F

uflip :: ((a, b) -> c) -> ((b, a) -> c)
uflip f = g
    where g p = f (swap p)


> Ejercicio 2:

-- 2.A

apply first :: (a, b) -> a

apply :: (a -> b) -> (a -> b)
    a <- (a, b)
    b <- a
first :: (a, b) -> a

-- 2.B

first (swap, uflip) :: (a, b) -> (b, a)

first :: (a, b) -> a
    a <- (a, b) -> (b, a)
    b <- ((a, b) -> c) -> ((b, a) -> c)
swap :: (a, b) -> (b, a)
uflip :: ((a, b) -> c) -> ((b, a) -> c)

-- 2.C

twice doble :: Int -> Int

twice :: (a -> a) -> (a -> a)
    a <- Int
doble :: Int -> Int

-- 2.D

twice twice :: (a' -> a')   /   twice twice :: (a -> a) -> (a -> a)

twice :: (a' -> a') -> (a' -> a')
    a' <- (a -> a)
twice :: (a -> a) -> (a -> a)

-- 2.E

twice uflip :: ((a, a) -> b) -> ((a, a) -> b)

twice :: (a -> a) -> (a -> a)
    a input  <- ((a, b) -> c)
    a output <- ((b, a) -> c)
    -- Se restringe el tipo de (a, b) y (b, a) al tipo (a, a).
uflip :: ((a, b) -> c) -> ((b, a) -> c)

-- 2.F

twice swap :: (a, a) -> (a, a)

twice :: (a -> a) -> (a -> a)
    a input  <- (a, b)
    a output <- (b, a)
    -- Se restringe el tipo de (a, b) y (b, a) al tipo (a, a).
swap :: (a, b) -> (b, a)

-- 2.G

uflip swap :: (b, a) -> (b, a)   /   uflip swap :: (a, b) -> (a, b)

uflip :: ((a, b) -> c) -> ((b, a) -> c)
    a <- a
    b <- b
    c <- (b, a)
swap :: (a, b) -> (b, a)

-- 2.H

(twice twice) swap :: (a, a) -> (a, a)

twice twice :: (a -> a) -> (a -> a)
    a input  <- (a, b)
    a output <- (b, a)
    -- Se restringe el tipo de (a, b) y (b, a) al tipo (a, a).
swap :: (a, b) -> (b, a)


> Ejercicio 3:

-- 3.A

const :: a -> (b -> a) -- Es la opción VII.
const x = g
    where g y = x

-- 3.B

appDup :: ((a, a) -> b) -> (a -> b) -- Es la opción II.
appDup f = g
    where g x = f (x , x)

-- 3.C

appFork :: (a -> b , a -> c) -> (a -> (b , c)) -- Es la opción V.
appFork (f, g) = h
    where h x = (f x, g x)

-- 3.D

appPar :: (a -> b , c -> d) -> ((a , c) -> (b , d)) -- Es la opción I.
appPar (f, g) = h
    where h (x, y) = (f x, g y)

-- 3.E

appDist :: (a -> b) -> ((a , a) -> (b , b)) -- Es la opción IV.
appDist f = g
    where g (x, y) = (f x, f y)

-- 3.F

flip :: (a -> (b -> c)) -> (b -> (a -> c)) -- Es la opción III.
flip f = h
    where h x = k
        where k y = (f y) x

-- 3.G

subst :: (a -> (b -> c)) -> ((a -> b) -> (a -> c)) -- Es la opción VI.
subst f = h
    where h g = k
        where k x = (f x) (g x)


> Ejercicio 4:



> Ejercicio 5:

-- 5.A

50 == 10
True && not False

-- 5.B

(6, 8)
(doble 3, cuadruple 2)

-- 5.C

(\c -> if c == 'T' then 1 else 0)
(\c1 c2 -> length (c1:c2:[])) 'b'

-- 5.D

...
...

-- 5.E

...
...

-- 5.F

...
...

-- 5.G

...
...


> Ejercicio 6:

