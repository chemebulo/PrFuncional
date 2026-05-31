--------------------------------------------------------------------------------------------------------

--                      TRABAJO PRÁCTICO N°8: Inducción y Recursión Estructural II                    --

--------------------------------------------------------------------------------------------------------

## SECCIÓN 1


> Ejercicio 1:

-- 1.A

length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs


-- 1.B

sum :: [Int] -> Int
sum []     = 0
sum (n:ns) = n + sum ns


-- 1.C

product :: [Int] -> Int
product []     = 0
product (n:ns) = n * product ns


-- 1.D

concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss


-- 1.E

elem :: Eq a => a -> [a] -> Bool
elem x []     = False
elem x (y:ys) = x == y || elem x ys


-- 1.F

all :: (a -> Bool) -> [a] -> Bool
all f []     = True
all f (x:xs) = f x && all f xs


-- 1.G

any :: (a -> Bool) -> [a] -> Bool
any f []     = False
any f (x:xs) = f x || any f xs


-- 1.H

count :: (a -> Bool) -> [a] -> Int
count f []     = 0
count f (x:xs) = unoSi f x + count f xs

unoSi :: (a -> Bool) -> a -> Int
unoSi f x = if f x then 1 else 0


-- 1.I

subset :: Eq a => [a] -> [a] -> Bool
subset []     ys = True
subset (x:xs) ys = elem x ys && subset xs ys


-- 1.J

(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : ((++) xs ys)


-- 1.K

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]


-- 1.L

zip :: [a] -> [b] -> [(a, b)]
zip []     _      = []
zip _      []     = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys


-- 1.M

unzip :: [(a, b)] -> ([a], [b])
unzip []           = ([], [])
unzip ((x, y):xys) = merge x y (unzip' xys)

merge :: a -> b -> ([a], [b]) -> ([a], [b])
merge x y (xs, ys) = (x:xs, y:ys)



> Ejercicio 2:

-- 2.A

¿Para todo xs. para todo ys. length (xs ++ ys) = length xs + length ys?

Demostración:
    Sean zs y ws listas cualquiera (finitas y totalmente definidas). Por principio de inducción en la estructura
    de zs es equivalente demostrar:

    Caso base (zs = []):
        ¿length ([] ++ ws) = length [] + length ws?

    Caso inductivo (zs = (z:zs')):
        Hipotesis inductiva:
            ¡length (zs' ++ ws) = length zs' + length ws!
        
        Tesis inductiva:
            ¿length ((z:zs') ++ ws) = length (z:zs') + length ws?

    Demostración caso base:
        ¿length ([] ++ ws) = length [] + length ws?

    -- LADO IZQUIERDO

        length ([] ++ ws)
    =                               ((++))
        length ((++) [] ws)
    =                               ((++).1)
        length (ws)
    =                               (aritmética)
        length ws

    -- LADO DERECHO

        length [] + length ws
    =                               (length.1)
        0 + length ws
    =                               (aritmética)
        length ws

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿length ((z:zs') ++ ws) = length (z:zs') + length ws?

    -- LADO IZQUIERDO

        length ((z:zs') ++ ws)
    =                               ((++))
        length ((++) (z:zs') ws)
    =                               ((++).2)
        length (z : ((++) zs' ws))
    =                               ((++))
        length (z : (zs' ++ ws))
    =                               (length.2)
        1 + length (zs' ++ ws)
    =                               (HI)
        1 + (length zs' + length ws)

    -- LADO DERECHO

        length (z:zs') + length ws
    =                               (length.2)
        1 + length zs' + length ws
    =                               (aritmética)
        1 + (length zs' + length ws)

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.B

¿Para todo xs. para todo ys. para todo zs. (xs ++ ys) ++ zs = xs ++ (ys ++ zs)?

Demostración:
    Sea as, bs y cs listas cualquiera (finitas y bien definidas). Por principio de inducción en la estructura
    de as es equivalente demostrar:

    Caso base (as = []):
        ¿([] ++ bs) ++ cs = [] ++ (bs ++ cs)?

    Caso inductivo (as = (a:as')):
        Hipotesis inductiva:
            ¡(as' ++ bs) ++ cs = as' ++ (bs ++ cs)!

        Tesis inductiva:
            ¿((a:as') ++ bs) ++ cs = (a:as') ++ (bs ++ cs)?

    Demostración caso base:
        ¿([] ++ bs) ++ cs = [] ++ (bs ++ cs)?

    -- LADO IZQUIERDO

        ([] ++ bs) ++ cs
    =                           ((++))
        ((++) [] bs) ++ cs
    =                           ((++).1)
        (bs) ++ cs
    =                           (aritmética)
        bs ++ cs

    -- LADO DERECHO

        [] ++ (bs ++ cs)
    =                           ((++))
        (++) [] (bs ++ cs)
    =                           ((++).1)
        (bs ++ cs)
    =                           (aritmética)
        bs ++ cs

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿((a:as') ++ bs) ++ cs = (a:as') ++ (bs ++ cs)? -- ¿Tiene sentido pasar toda la demostración al prefijo (++)?

    -- LADO IZQUIERDO

        ((a:as') ++ bs) ++ cs
    =                               ((++))
        ((++) (a:as') bs) ++ cs
    =                               ((++))
        (++) ((++) (a:as') bs) cs
    =                               ((++).2)
        (++) (a : ((++) as' bs)) cs
    =                               ((++).2)
        a : ((++) ((++) as' bs) cs)
    =                               ((++))
        a : (((++) as' bs) ++ cs)
    =                               ((++))
        a : ((as' ++ bs) ++ cs)
    =                               (HI)
        a : (as' ++ (bs ++ cs))
    =                               (aritmética)
        a : as' ++ (bs ++ cs)

    -- LADO DERECHO

        (a:as') ++ (bs ++ cs)
    =                               ((++))
        (++) (a:as') (bs ++ cs)
    =                               ((++).2)
        a : ((++) as' (bs ++ cs))
    =                               ((++))
        a : (as' ++ (bs ++ cs))
    =                               (aritmética)
        a : as' ++ (bs ++ cs)

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.C

count (const True) = length

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que 
    ¿para todo xs. count (const True) xs = length xs?

    Sea as una lista cualquiera (finita y bien definida). Por principio de inducción en la estructura as
    es equivalente demostrar:

    Caso base (as = [])
        ¿count (const True) [] = length []? 

    Caso inductivo (as = (a:as'))
        Hipotesis inductiva:
            ¡count (const True) as' = length as'!

        Tesis inductiva:
            ¿count (const True) (a:as') = length (a:as')?

    Demostración caso base:
        ¿count (const True) [] = length []? 

    -- LADO IZQUIERDO

        count (const True) []
    =                               (count.1)
        0

    -- LADO DERECHO

        length []
    =                               (length.1)
        0

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿count (const True) (a:as') = length (a:as')?

    -- LADO IZQUIERDO

        count (const True) (a:as')
    =                                                   (count.2)
        unoSi (const True) a + count (const True) as'
    =                                                   (unoSi.1)
        1 + count (const True) as'
    =                                                   (HI)
        1 + length as'

    -- LADO DERECHO

        length (a:as')
    =                                                   (length.2)
        1 + length as'

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.D

elem = any . (==)

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿para todo x. para todo xs. elem x xs = any . (==) x xs?

    Sea as una lista (finita y bien definida) y b un elemento del mismo tipo de la lista. Por principio de inducción 
    en la estructura as es equivalente demostrar que:

    Caso base (as = []):
        ¿elem b [] = any . (==) b []?

    Caso inductivo (as = (a:as')):
        Hipotesis inductiva:
            ¡elem b as' = any . (==) b as'!

        Tesis inductiva:
            ¿elem b (a:as') = any . (==) b (a:as')?

    Demostración caso base:
        ¿elem b [] = any . (==) b []?

    -- LADO IZQUIERDO

        elem b []
    =                       (elem.1)
        False

    -- LADO DERECHO

        any . (==) b []
    =                       (.)
        any ((==) b) []
    =                       (any.1)
        False

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿elem b (a:as') = any . (==) b (a:as')?

    -- LADO IZQUIERDO

        elem b (a:as')
    =                                   (elem.2)
        b == a || elem b as'
    =                                   (HI)
        b == a || any . (==) b as'
    =                                   ((==))
        (==) b a || any . (==) b as'
    =                                   (.)
        (==) b a || any ((==) b) as'

    -- LADO DERECHO

        any . (==) b (a:as')
    =                                   (.)
        any ((==) b) (a:as')
    =                                   (any.2)
        (==) b a || any ((==) b) as'

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

-- 2.E



-- 2.F



-- 2.G



-- 2.H



-- 2.I



-- 2.J



-- 2.K


## SECCIÓN 2


> Ejercicio 1:

data N = Z | S N

-- 1.A.I

evalN :: N -> Int
evalN Z     = 0
evalN (S n) = 1 + evalN n


-- 1.A.II

addN :: N -> N -> N
addN Z     m = m
addN (S n) m = S (addN n m)


-- 1.A.III

prodN :: N -> N -> N
prodN Z     _ = Z
prodN (S n) m = addN m (prodN n m)


-- 1.A.IV

int2N  ::  Int  ->  N
int2N 0 = Z
int2N n = S (int2N (n-1))


-- 1.B.I

¿Para todo n1. para todo n2. evalN (addN n1 n2) = evalN n1 + evalN n2?



-- 1.B.II

¿Para todo n1. para todo n2. evalN (prodN n1 n2) = evalN n1 * evalN n2?



-- 1.B.III

int2N . evalN = id



-- 1.B.IV

evalN . int2N = id 




> Ejercicio 2:

type NU = [()]
data Unit = Unit

-- 2.A.I

evalNU :: NU -> Int
evalNU []     =
evalNU (u:us) =


-- 2.A.II

succNU :: NU -> NU
evalNU []     =
evalNU (u:us) =


-- 2.A.III

addNU :: NU -> NU -> NU
addNU []     n =
addNU (u:us) n =

-- 2.A.IV

nu2n :: NU -> N
nu2n []     =
nu2n (u:us) =


-- 2.A.V

n2nu :: N -> NU
n2nu Z     =
n2nu (S n) =


-- 2.B.I

evalNU . succNU = (+1) . evalNU 


-- 2.B.II

Para todo n1. para todo n2. evalNU (addNU n1 n2) = evalNU n1 + evalNU n2 


-- 2.B.III

nu2n . n2nu = id 

-- 2.B.IV

n2nu . nu2n = id