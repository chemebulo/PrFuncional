--------------------------------------------------------------------------------------------------------

--                                 TRABAJO PRÁCTICO N°3: Currificación                                --

--------------------------------------------------------------------------------------------------------

> Ejercicio 1:

-- 1.A

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

curry :: ((a, b) -> c) -> a -> b -> c
curry f = g
    where g x = h
        where h y = f (x, y)

-- 1.B

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y 

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = g
    where g (x, y) = f x y


> Ejercicio 2:

-- 2.A

apply f x = f x

-- 2.B

twice f x = f (f x)

-- 2.C

id x = x

-- 2.D

flip f x y = (f y) x

-- 2.E

uflip f p = f (swap p)

-- 2.F

const x y = x

-- 2.G

compose f g x = f (g x)


> Ejercicio 3:

-- 3.A

apply :: (a -> b) -> a -> b

-- 3.B

twice :: (a -> a) -> a -> a

-- 3.C

id :: a -> a

-- 3.D

flip :: (a -> b -> c) -> b -> a -> c

-- 3.E

uflip :: ((a, b) -> c) -> (b, a) -> c

-- 3.F

const :: a -> b -> a

-- 3.G

compose :: (b -> c) -> (a -> b) -> a -> c


> Ejercicio 4:

-- 4.A

(apply apply) apply :: (a -> b) -> a -> b

-- 4.B

(twice doble) 2 :: Int

-- 4.C

((twice twice) twice) swap :: (a, a) -> (a, a)

-- 4.D

((flip twice) 1) doble :: Int


> Ejercicio 5:

-- 5.A

appDup f = g 
    where g x = f (x, x) 

appDup = 

-- 5.B

appFork (f, g) = h 
    where h x = (f x, g x) 

appFork = 

-- 5.C

appPar (f, g) = h 
    where h (x, y) = (f x, g y) 

appPar = 

-- 5.D

appDist f = g 
    where g (x, y) = (f x, f y) 

appDist = 

-- 5.E

subst f = h 
    where h g = k 
        where k x = (f x) (g x)

subst = 

> Ejercicio 6:

