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
product []     = 1
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
count f (x:xs) = unoSi (f x) + count f xs

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

-- 1.I

subset :: Eq a => [a] -> [a] -> Bool
subset []     ys = True
subset (x:xs) ys = elem x ys && subset xs ys

-- 1.J

append ::  [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys 

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
unzip []       = ([], [])
unzip (xy:xys) = merge xy (unzip xys) 

merge :: (a, b) -> ([a], [b]) -> ([a], [b])
merge (x, y) (xs, ys) = (x:xs, y:ys)


> Ejercicio 2:

-- 2.A

¿para todo xs. para todo ys. length (xs ++ ys) = length xs + length ys?

Demostración:
    Sea zs :: [a], sea ws :: [a]. Por principio de inducción en la estructura
    de zs es equivalente demostrar que:

    Caso base (zs = []):
        ¿length ([] ++ ws) = length [] + length ws?

    Caso inductivo (zs = (z:zs')):
        Hipotesis inductiva:
            ¡length (zs' ++ ws) = length zs' + length ws!

        Tesis inductiva::
            ¿length ((z:zs') ++ ws) = length (z:zs') + length ws?

    Demostración caso base:
        ¿length ([] ++ ws) = length [] + length ws?

    -- LADO IZQUIERDO:

        length ([] ++ ws)
    =                                   ((++).1)
        length ws

    -- LADO DERECHO:

        length [] + length ws
    =                                   (length.1)
        0 + length ws
    =                                   (aritmética)
        length ws
    
    -- Ambos lados llega a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿length ((z:zs') ++ ws) = length (z:zs') + length ws?

    -- LADO IZQUIERDO:

        length ((z:zs') ++ ws)
    =                                   ((++).2)
        length (z : (zs' ++ ws))
    =                                   (length.2)
        1 + length (zs' ++ ws)
    =                                   (HI)
        1 + length zs' + length ws

    -- LADO DERECHO:

        length (z:zs') + length ws
    =                                   (length.2)
        1 + length zs' + length ws

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.B

¿para todo xs. para todo ys. para todo zs. (xs ++ ys) ++ zs = xs ++ (ys ++ zs)?

Demostración:
    Sea xs' :: [a], sea ys' :: [a], sea zs' :: [a]. Por principio de inducción en la estructura
    de xs' es equivalente demostrar que:

    Caso base (xs' = []):
        ¿([] ++ ys') ++ zs' = [] ++ (ys' ++ zs')?

    Caso inductivo (xs' = (x:xs'')):
        Hipotesis inductiva:
            ¡(xs'' ++ ys') ++ zs' = xs'' ++ (ys' ++ zs')!

        Tesis inductiva:
            ¿((x:xs'') ++ ys') ++ zs' = (x:xs'') ++ (ys' ++ zs')?

    Demostración caso base:
        ¿([] ++ ys') ++ zs' = [] ++ (ys' ++ zs')?

    -- LADO IZQUIERDO:
        
        ([] ++ ys') ++ zs'
    =                                   ((++).1)
        ys' ++ zs'

    -- LADO DERECHO:
        
        [] ++ (ys' ++ zs')
    =                                   ((++).1)
        ys' ++ zs'

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿((x:xs'') ++ ys') ++ zs' = (x:xs'') ++ (ys' ++ zs')?

    -- LADO IZQUIERDO:

        ((x:xs'') ++ ys') ++ zs'
    =                                   ((++).2)
        (x : (xs'' ++ ys')) ++ zs'
    =                                   ((++).2)
        x : ((xs'' ++ ys') ++ zs')
    =                                   (aritmética)
        x : (xs'' ++ (ys' ++ zs'))


    -- LADO DERECHO:

        (x:xs'') ++ (ys' ++ zs')
    =                                   ((++).2)
        x : (xs'' ++ (ys' ++ zs'))

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.C

¿count (const True) = length?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que:
    ¿para todo xs. count (const True) xs = length xs?

    Sea ys :: [a]. Por principio de inducción en la estructura
    de ys es equivalente demostrar que:

    Caso base (ys = []):
        ¿count (const True) [] = length []?

    Caso inductivo (ys = (y:ys'))
        Hipotesis inductiva:
            ¡count (const True) ys' = length ys'!

        Tesis inductiva:
            ¿count (const True) (y:ys') = length (y:ys')?
    
    Demotración caso base:
        ¿count (const True) [] = length []?

    -- LADO IZQUIERDO:

        count (const True) []
    =                               (count.1)
        0

    -- LADO DERECHO:

        length []
    =                               (length.1)
        0

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿count (const True) (y:ys') = length (y:ys')?

    -- LADO IZQUIERDO:

        count (const True) (y:ys')
    =                                                        (count.2)
        unoSi ((const True) y) + count (const True) ys' 
    =                                                        (const, x <- True, y <- y)
        unoSi True + count (const True) ys'
    =                                                        (unoSi.1)
        1 + count (const True) ys'
    =                                                        (HI)
        1 + length ys'

    -- LADO DERECHO:

        length (y:ys')
    =                                                       (length.2)
        1 + length ys'

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.D

¿elem = any . (==)?

Demostración:
    Por principio de extensionalidad (dos veces), es equivalente demostrar que:
    ¿para todo x. para todo xs. elem x xs = (any . (==)) x xs?

    Por definición de (.) es equivalente demostrar que:
    ¿para todo x. para todo xs. elem x xs = any ((==) x) xs?

    Sea z :: a, sea ys :: [a]. Por principio de inducción en la estructura
    de ys es equivalente demostrar:

    Caso base (ys = []):
        ¿elem z [] = any ((==) z) []?

    Caso inductivo (ys = (y:ys')):
        Hipotesis inductiva:
            ¡elem z ys' = any ((==) z) ys'!

        Tesis inductiva:
            ¿elem z (y:ys') = any ((==) z) (y:ys')?

    Demostración caso base:
        ¿elem z [] = any ((==) z) []?

    -- LADO IZQUIERDO:

        elem z []
    =                                   (elem.1)
        False

    -- LADO DERECHO:

        any ((==) z) []
    =                                   (any.1)
        False

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿elem z (y:ys') = any ((==) z) (y:ys')?

    -- LADO IZQUIERDO:

        elem z (y:ys')
    =                                   (elem.2)
        z == y || elem z ys'

    -- LADO DERECHO:

        any ((==) z) (y:ys')
    =                                   (any.2)
        (==) z y || any ((==) z) ys'     
    =                                   (HI)
        (==) z y  || elem z ys'
    =                                   ((==))
        z == y || elem z ys'

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.E

¿para todo x. any (elem x) = elem x . concat?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que:
    ¿para todo x. para todo xss. any (elem x) xss = (elem x . concat) xss?

    Por definición de (.) es equivalente demostrar que:
    ¿para todo x. para todo xss. any (elem x) xss = elem x (concat xss)?

    Sea z :: a, sea yss :: [[a]]. Por principio de inducción en la estructura
    de yss es equivalente demostrar:

    Caso base (yss = []):
        ¿any (elem z) [] = elem z (concat [])?

    Caso inductivo (yss = (ys:yss')):  
        Hipotesis inductiva:
            ¡any (elem z) yss' = elem z (concat yss')!

        Tesis inductiva:
            ¿any (elem z) (ys:yss') = elem z (concat (ys:yss'))?

    Demostración caso base:
        ¿any (elem z) [] = elem z (concat [])?

    -- LADO IZQUIERDO:

        any (elem z) []
    =                           (any.1)
        False

    -- LADO IZQUIERDO:

        elem z (concat [])
    =                           (concat.1)
        elem z []
    =                           (elem.1)
        False

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿any (elem z) (ys:yss') = elem z (concat (ys:yss'))?

    -- LADO IZQUIERDO:

        any (elem z) (ys:yss')
    =                                           (any.2)
        elem z ys || any (elem z) yss'
    =                                           (HI)
        elem z ys || elem z (concat yss')

    -- LADO IZQUIERDO:

        elem z (concat (ys:yss'))
    =                                           (concat.2)
        elem z (ys ++ concat yss')
    =                                           (ElemAppend)
        elem z ys || elem z (concat yss')

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


    Lema ElemAppend: ¿para todo z. para todo xs. para todo ys. elem z (xs ++ ys) = elem z xs || elem z ys?
    
    Demostración:
        Sea x :: a, sea ws :: [a], sea ts :: [a]. Por principio de inducción en la estructura
        de ws es equivalente demostrar:

        Caso base (ws = []):
            ¿elem x ([] ++ ts) = elem x [] || elem x ts?

        Caso inductivo (ws = (w:ws')):
            Hipotesis inductiva:
                ¡elem x (ws' ++ ts) = elem x ws' || elem x ts!

            Tesis inductiva:
                ¿elem x ((w:ws') ++ ts) = elem x (w:ws') || elem x ts?

        Demostración caso base:
            ¿elem x ([] ++ ts) = elem x [] || elem x ts?

        -- LADO IZQUIERDO:

            elem x ([] ++ ts)
        =                                       ((++).1)
            elem x ts

        -- LADO DERECHO:

            elem x [] || elem x ts
        =                                       ((++).1)
            False || elem x ts
        =                                       (||)
            elem x ts

        -- Ambos lados llegan a lo mismo, el caso es válido.

        Demostración caso inductivo:
            ¿elem x ((w:ws') ++ ts) = elem x (w:ws') || elem x ts?

        -- LADO IZQUIERDO:

            elem x ((w:ws') ++ ts)
        =                                       ((++).2)
            elem x (w : (ws' ++ ts))
        =                                       (elem.2)
            x == w || elem x (ws' ++ ts)
        =                                       (HI)
            x == w || elem x ws' || elem x ts

        -- LADO DERECHO:

            elem x (w:ws') || elem x ts
        =                                       (elem.2)
            x == w || elem x ws' || elem x ts

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.F

¿para todo xs. para todo ys. subset xs ys = all (flip elem ys) xs?

Demostración:
    Sea ws :: [a], sea zs :: [a]. Por principio de inducción en la estructura
    de ws es equivalente demostrar:

    Caso base (ws = []):
        ¿subset [] zs = all (flip elem zs) []?

    Caso inductivo (ws = (w:ws')):
        Hipotesis inductiva:
            ¡subset ws' zs = all (flip elem zs) ws'!

        Tesis inductiva:
            ¿subset (w:ws') zs = all (flip elem zs) (w:ws')?

    Demostración caso base:
        ¿subset [] zs = all (flip elem zs) []?

    -- LADO IZQUIERDO:

        subset [] zs
    =                                           (subset.1)
        True

    -- LADO DERECHO:

        all (flip elem zs) []
    =                                           (all.1)
        True

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿subset (w:ws') zs = all (flip elem zs) (w:ws')?

    -- LADO IZQUIERDO:

        subset (w:ws') zs
    =                                               (subset.2)
        elem w zs && subset ws' zs 
    =                                               (HI)
        elem w zs && all (flip elem zs) ws'

    -- LADO DERECHO:

        all (flip elem zs) (w:ws')
    =                                               (all.2)
        (flip elem zs) w && all (flip elem zs) ws'
    =                                               (flip, f <- elem, x <- zs, y <- w)
        elem w zs && all (flip elem zs) ws'

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.G

¿all null = null . concat?

Demostración:
    a

-- 2.H

¿length = length . reverse?

Demostración:
    a


-- 2.I

¿para todo xs. para todo ys. reverse (xs ++ ys) = reverse ys ++ reverse xs?

Demostración:
    a

-- 2.J

¿para todo xs. para todo ys. all p (xs ++ ys) = all p (reverse xs) && all p (reverse ys)?

Demostración:
    a

-- 2.K

¿para todo xs. para todo ys. unzip (zip xs ys) = (xs, ys)?

Demostración:
    a


#############################################################################################################################

## SECCIÓN 2


#############################################################################################################################

## SECCIÓN 3

