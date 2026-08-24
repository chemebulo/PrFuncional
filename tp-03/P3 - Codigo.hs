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

appDup :: ((a, a) -> b) -> (a -> b)
appDup f = g 
    where g x = f (x, x) 

appDup :: ((a, a) -> b) -> (a -> b)
appDup = \f -> (\x -> f (x, x))

-- 5.B

appFork :: (a -> b, a -> c) -> (a -> (b, c))
appFork (f, g) = h 
    where h x = (f x, g x) 

appFork :: (a -> b, a -> c) -> (a -> (b, c))
appFork = \(f, g) -> (\x -> (f x, g x))

-- 5.C

appPar :: (a -> b, c -> d) -> ((a, c) -> (b, d))
appPar (f, g) = h 
    where h (x, y) = (f x, g y) 

appPar :: (a -> b, c -> d) -> ((a, c) -> (b, d))
appPar = \(f, g) -> (\(x, y) -> (f x, g y))

-- 5.D

appDist :: (a -> b) -> ((a, a) -> (b, b))
appDist f = g 
    where g (x, y) = (f x, f y) 

appDist :: (a -> b) -> ((a, a) -> (b, b))
appDist = \f -> (\(x, y) -> (f x, f y))

-- 5.E

subst :: (a -> b -> c) -> ((a -> b) -> a -> c)
subst f = h 
    where h g = k 
        where k x = (f x) (g x)

subst :: (a -> b -> c) -> ((a -> b) -> a -> c)
subst = \f -> (\g -> (\x -> (f x) (g x)))


> Ejercicio 6:

-- 6.A

compose (fst snd) :: No tiene tipo.
(compose fst) snd :: (a, (b, c)) -> b

-- 6.B

(uncurry curry snd) :: No tiene tipo.
uncurry (curry snd) :: (a, b) -> b

-- 6.C

(apply id) ((id apply) apply) :: (a -> b) -> a -> b

-- 6.D

compose (compose doble doble) :: (a -> Int) -> a -> Int

-- 6.E

(compose compose) doble doble :: No tiene tipo.
compose (compose doble doble) :: (a -> Int) -> a -> Int


> Ejercicio 7:

many :: Int -> (a -> a) -> a -> a
many 0 f x = x
many n f x = f (many (n-1) f x)

many :: Int -> (a -> a) -> a -> a
many 0 f = id
many n f = f . (many (n-1) f)


> Ejercicio 8:

-- 8.A

(Int -> Int) -> Int -> Int
-- Castellano: Es una función que toma una función que toma un entero y devuelve un entero, y devuelve una función que toma
--             un entero y devuelve un entero.
-- Frances: Es una función que toma una función que toma un entero y devuelve un entero, un entero y devuelve un entero.

-- 8.B

(a -> b -> c) -> a -> b -> c 
-- Castellano: Es una función que toma una función y devuelve una función que toma un elemento de tipo a, que devuelve una
--             función que toma un elemento de tipo b, que devuelve un elemento de tipo c.
-- Frances: Es una función que toma una función, un elemento de tipo a, un elemento de tipo b, y devuelve un elemento de tipo c.

-- 8.C

(a -> b, c -> d) -> (a, c) -> (b, d)
-- Castellano: Es una función que toma un par compuesto de funciones, que devuelve una función que toma un par, que devuelve un par.
-- Frances: Es una función que toma un par compuesto funciones, un par y devuelve un par.

-- 8.D

((a, a) -> b) -> a -> b 
-- Castellano: Es una función que toma una función y devuelve una función que toma un elemento de tipo a y devuelve un elemento de tipo b.
-- Frances: Es una función que toma una función, un elemento de tipo a, y devuelve un elemento de tipo b.

-- 8.E

(a -> b -> c) -> b -> a -> c 
-- Castellano: Es una función que toma una función y devuelve una función que toma un elemento de tipo b, que devuelve una función
--             que toma un elemento de tipo a y devuelve un elemento de tipo c.
-- Frances: Es una función que toma una función, un elemento de tipo b, un elemento de tipo a, y devuelve un elemento de tipo c.

-- 8.F

(a -> b) -> (a, a) -> (b, b)
-- Castellano: Es una función que toma una función y devuelve una función que toma un par, y devuelve un par.
-- Frances: Es una función que toma una función, un par, y devuelve un par.

-- 8.G

(a -> b, a -> c) -> a -> (b, c)
-- Castellano: Es una función que toma un par compuesto por funciones, y devuelve una función que toma un elemento de tipo a, y
--             devuelve un par.
-- Frances: Es una función que toma un par compuesto por funciones, un elemento de tipo a, y devuelve un par.

-- 8.H

(a -> b -> c) -> (a -> b) -> a -> c 
-- Castellano: Es una función que toma una función, que devuelve una función que toma una función, devuelve una función que
--             toma un elemento de tipo a, y devuelve un elemento de tipo c.
-- Frances: Es una función que toma dos funciones, un elemento de tipo a, y devuelve un elemento de tipo c.

-- 8.I

a -> b -> a
-- Castellano: Es una función que toma un elemento de tipo a, y devuelve una función que toma un elemento de tipo b y devuelve
--             un elemento de tipo a.
-- Frances: Es una función que dado un elemento de tipo a, y un elemento de tipo b, devuelve el elemento de tipo a.


> Ejercicio 9:

-- 9.A

cuadruple x = doble (doble x)
cuadruple x = compose doble doble x
cuadruple x = apply ((twice doble) . (+0)) x
cuadruple x = flip (+) (doble x) (doble x) 

-- 9.B

timesTwoPlusThree x = suma (doble x) 3
timesTwoPlusThree x = flip suma (doble x) 3
timesTwoPlusThree x = suma (suma x x) 3
timesTwoPlusThree x = (3+) (subst (+) id x)

-- 9.C

fourTimes f x = f (f (f (f x)))
fourTimes f x = many 4 f x
fourTimes f x = (twice twice) f x
fourTimes f x = (twice . twice) f x


> Ejercicio 10:

Es una sintaxis especial para aplicación parcial en operadores infijos.
    La notación infijo es aquella donde el nombre se encuentra entre sus argumentos.
    La notación prefijo es aquella donde el nombre se encuentra antes de sus argumentos.

Al poner entre paréntesis el operador y uno de sus argumentos devuelve una función
intermedia que espera el segundo argumento.

- Sección Izquierda
    (2*)
    (*) 2

- Sección Derecha
    (*2)
    flip (*) 2

- Backticks
    foldr (+) 0
    (+) `foldr` 0