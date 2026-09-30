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
    xs' es equivalente demostrar que:

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
    ys es equivalente demostrar que:

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
    
    



-- 2.E

¿para todo x. any (elem x) = elem x . concat?


-- 2.F

¿para todo xs. para todo ys. subset xs ys = all (flip elem ys) xs?


-- 2.G

¿all null = null . concat?


-- 2.H

¿length = length . reverse?


-- 2.I

¿para todo xs. para todo ys. reverse (xs ++ ys) = reverse ys ++ reverse xs?


-- 2.J

¿para todo xs. para todo ys. all p (xs ++ ys) = all p (reverse xs) && all p (reverse ys)?


-- 2.K

¿para todo xs. para todo ys. unzip (zip xs ys) = (xs, ys)?



#############################################################################################################################

## SECCIÓN 2


#############################################################################################################################

## SECCIÓN 3

