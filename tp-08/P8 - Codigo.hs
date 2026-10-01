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
        flip elem zs w && all (flip elem zs) ws'
    =                                               (flip, f <- elem, x <- zs, y <- w)
        elem w zs && all (flip elem zs) ws'

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.G

¿all null = null . concat?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que:
    ¿para todo xss. all null xss = (null . concat) xss?

    Por definición de (.) es equivalente demostrar que:
    ¿para todo xss. all null xss = null (concat xss)?

    Sea wss :: [[a]]. Por principio de inducción en la estructura
    de wss es equivalente demostrar:

    Caso base (wss = []):
        ¿all null [] = null (concat [])?

    Caso inductivo (wss = (ws:wss')):
        Hipotesis inductiva:
            ¡all null wss' = null (concat wss')!

        Tesis inductiva:
            ¿all null (ws:wss') = null (concat (ws:wss'))?

    Demostración caso base:
        ¿all null [] = null (concat [])?

    -- LADO IZQUIERDO:
    
        all null []
    =                           (all.1)
        True

    -- LADO DERECHO:
    
        null (concat [])
    =                           (concat.1)
        null []
    =                           (null.1)
        True

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿all null (ws:wss') = null (concat (ws:wss'))?

    -- LADO IZQUIERDO:

        all null (ws:wss')
    =                                       (all.2)
        null ws && all null wss'
    =                                       (HI)
        null ws && null (concat wss')

    -- LADO DERECHO:

        null (concat (ws:wss'))
    =                                       (concat.2)
        null (ws ++ concat wss')
    =                                       (NullAppend)
        null ws && null (concat wss')

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema NullAppend: ¿para todo xs. para todo ys. null (xs ++ ys) = null xs && null ys?

    Demostración:
        Sea ws :: [a], sea zs :: [a]. Por principio de inducción en la estructura
        de ws es equivalente demostrar que:

        Caso base (ws = []):
            ¿null ([] ++ zs) = null [] && null zs?

        Caso inductivo (ws = (w:ws')):
            Hipotesis inductiva:
                ¡null (ws' ++ zs) = null ws' && null zs!

            Tesis inductiva:
                ¿null ((w:ws') ++ zs) = null (w:ws') && null zs?

        Demostración caso base:
            ¿null ([] ++ zs) = null [] && null zs?

        -- LADO IZQUIERDO:

            null ([] ++ zs)
        =                               ((++).1)
            null zs

        -- LADO DERECHO:

            null [] && null zs
        =                               (null.1)
            True && null zs
        =                               (&&)
            null zs

        -- Ambos lados llegan a lo mismo, el caso es válido.

        Demostración caso inductivo:
            ¿null ((w:ws') ++ zs) = null (w:ws') && null zs?

        -- LADO IZQUIERDO:

            null ((w:ws') ++ zs)
        =                               ((++).2)
            null (w : (ws' ++ zs))
        =                               (null.2)
            False

        -- LADO DERECHO:

            null (w:ws') && null zs
        =                               (null.2)
            False && null zs
        =                               (&&)
            False

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.H

¿length = length . reverse?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que:
    ¿para todo xs. length xs = (length . reverse) xs?

    Por definición de (.) es equivalente demostrar que:
    ¿para todo xs. length xs = length (reverse xs)?
    
    Sea ws :: [a]. Por principio de inducción en la estructura
    de ws es equivalente demostrar:

    Caso base (ws = []):
        ¿length [] = length (reverse [])?

    Caso inductivo (ws = (w:ws')):
        Hipotesis inductiva:
            ¡length ws' = length (reverse ws')!

        Tesis inductiva:
            ¿length (w:ws') = length (reverse (w:ws'))?

    Demostración caso base:
        ¿length [] = length (reverse [])?

    -- LADO IZQUIERDO:

        length []

    -- LADO DERECHO:

        length (reverse [])
    =                                   (reverse.1)
        length []

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿length (w:ws') = length (reverse (w:ws'))?

    -- LADO IZQUIERDO:

        length (w:ws')
    =                                           (length.2)
        1 + length ws'
    =                                           (HI)
        1 + length (reverse ws')

    -- LADO DERECHO:
    
        length (reverse (w:ws'))
    =                                           (reverse.2)
        length (reverse ws' ++ [w])
    =                                           (Propiedad demostrada en S1.2.A)
        length (reverse ws') + length (w:[])
    =                                           (length.2)
        length (reverse ws') + 1 + length []
    =                                           (length.1)
        length (reverse ws') + 1 + 0
    =                                           (aritmética)
        1 + length (reverse ws')

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.I

¿para todo xs. para todo ys. reverse (xs ++ ys) = reverse ys ++ reverse xs?

Demostración:
    Sea ws :: [a], sea zs :: [a]. Por principio de inducción en la estructura
    de ws es equivalente demostrar:

    Caso base (ws = []):
        ¿reverse ([] ++ zs) = reverse zs ++ reverse []?

    Caso inductivo (ws = (w:ws')):
        Hipotesis inductiva:
            ¡reverse (ws' ++ zs) = reverse zs ++ reverse ws'!

        Tesis inductiva:
            ¿reverse ((w:ws') ++ zs) = reverse zs ++ reverse (w:ws')?

    Demostración caso base:
        ¿reverse ([] ++ zs) = reverse zs ++ reverse []?

    -- LADO IZQUIERDO:

        reverse ([] ++ zs)
    =                                   ((++).1)
        reverse zs

    -- LADO DERECHO:

        reverse zs ++ reverse []
    =                                   (reverse.1)
        [] ++ reverse zs
    =                                   ((++).1)
        reverse zs

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿reverse ((w:ws') ++ zs) = reverse zs ++ reverse (w:ws')?

    -- LADO IZQUIERDO:

        reverse ((w:ws') ++ zs)
    =                                           ((++).2)
        reverse (w : (ws' ++ zs))
    =                                           (reverse.2)
        reverse (ws' ++ zs) ++ [w]
    =                                           (HI)
        reverse zs ++ reverse ws' ++ [w]

    -- LADO DERECHO:

        reverse zs ++ reverse (w:ws')
    =                                           (reverse.2)
        reverse zs ++ reverse ws' ++ [w]

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.J

¿para todo xs. para todo ys. all p (xs ++ ys) = all p (reverse xs) && all p (reverse ys)?

Demostración:
    Sea ws :: [a], sea zs :: [a]. Por principio de inducción en la estructura
    de ws es equivalente demostrar:

    Caso base (ws = []):
        ¿all p ([] ++ zs) = all p (reverse []) && all p (reverse zs)?

    Caso inductivo (ws = (w:ws')):
        Hipotesis inductiva:
            ¡all p (ws' ++ zs) = all p (reverse ws') && all p (reverse zs)!

        Tesis inductiva:
            ¿all p ((w:ws') ++ zs) = all p (reverse (w:ws')) && all p (reverse zs)?

    Demostración caso base:
        ¿all p ([] ++ zs) = all p (reverse []) && all p (reverse zs)?

    -- LADO IZQUIERDO:

        all p ([] ++ zs)
    =                                                   ((++).1)
        all p zs

    -- LADO DERECHO:

        all p (reverse []) && all p (reverse zs)
    =                                                   (reverse.1)
        all p [] && all p (reverse zs)
    =                                                   (all.1)
        True && all p (reverse zs)
    =                                                   (&&)
        all p (reverse zs)

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿all p ((w:ws') ++ zs) = all p (reverse (w:ws')) && all p (reverse zs)?
        
        ¡all p (ws' ++ zs) = all p (reverse ws') && all p (reverse zs)!

    -- LADO IZQUIERDO:

        all p ((w:ws') ++ zs)
    =                                                           ((++).2)
        all p (w : (ws' ++ zs))
    =                                                           (all.2)
        p w && all p (ws' ++ zs)
    =                                                           (HI)
        p w && all p (reverse ws') && all p (reverse zs)
    =                                                           (Lema AllReverse)
        p w && all p ws' && all p (reverse zs)

    -- LADO DERECHO:

        all p (reverse (w:ws')) && all p (reverse zs)
    =                                                           (Lema AllReverse)
        all p (w:ws') && all p (reverse zs)
    =                                                           (all.2)
        p w && all p ws' && all p (reverse zs)

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema AllReverse: ¿para todo xs. all p xs = all p (reverse xs)?

    Demostración:
        Sea ws :: [a]. Por principio de inducción en la estructura
        de ws es equivalente demostrar que:

        Caso base (ws = []):
            ¿all p [] = all p (reverse [])?

        Caso inductivo (ws = (w:ws')):
            Hipotesis inductiva:
                ¡all p ws' = all p (reverse ws')!
            
            Tesis inductiva:
                ¿all p (w:ws') = all p (reverse (w:ws'))?

        Demostración caso base:
            ¿all p [] = all p (reverse [])?

        -- LADO IZQUIERDO

            all p []

        -- LADO DERECHO

            all p (reverse [])
        =                       (reverse.1)
            all p []

        -- Ambos lados llegan a lo mismo, el caso es válido.

        Demostración caso inductivo:
            ¿all p (w:ws') = all p (reverse (w:ws'))?

        -- LADO IZQUIERDO

            all p (w:ws')
        =                                       (all.2)
            p w && all p ws'

        -- LADO DERECHO

            all p (reverse (w:ws'))
        =                                       (reverse.2)
            all p (reverse ws' ++ [w])
        =                                       (Lema AllDist)
            all p (reverse ws') && all p [w]
        =                                       (HI)
            all p ws' && all p (w:[])
        =                                       (all.2)
            all p ws' && p w && all p []
        =                                       (all.1)
            all p ws' && p w && True
        =                                       ((&&).1)
            p w && all p ws'

        -- Ambos lados legan a lo mismo, el caso es válido y la propiedad también.

    Lema AllDist: ¿para todo xs. para todo ys. all p (xs ++ ys) = all p xs && all p ys?

    Demostración:
        Sea ks :: [a], sea js :: [a]. Por principio de inducción en la estructura
        de ks es equivalente demostrar:

        Caso base (ks = []):
            ¿all p ([] ++ js) = all p [] && all p js?

        Caso inductivo (ks = (k:ks')):
            Hipotesis inductiva:
                ¡all p (ks' ++ js) = all p ks' && all p js!
            
            Tesis inductiva:
                ¿all p ((k:ks') ++ js) = all p (k:ks') && all p js?

        Demostración caso base:
            ¿all p ([] ++ js) = all p [] && all p js?

        -- LADO IZQUIERDO

            all p ([] ++ js)
        =                           ((++).1)
            all p js

        -- LADO DERECHO

            all p [] && all p js
        =                           (all.1)
            True && all p js
        =                           ((&&).1)
            all p js

        -- Ambos lados llegan a lo mismo, el caso es válido.

        Demostración caso inductivo:
            ¿all p ((k:ks') ++ js) = all p (k:ks') && all p js?

        -- LADO IZQUIERDO

            all p ((k:ks') ++ js)
        =                                   ((++).2)
            all p (k : (ks' ++ js))
        =                                   (all.2)
            p k && all p (ks' ++ js)
        =                                   (HI)
            p k && all p ks' && all p js

        -- LADO DERECHO

            all p (k:ks') && all p js
        =                                   (all.2)
            p k && all p ks' && all p js

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.K

¿para todo xs. para todo ys. unzip (zip xs ys) = (xs, ys)?

Demostración:
    Esta propiedad no se cumple para todo xs y para todo ys. Para demostrarlo, propongo el siguiente contraejemplo:
        xs = [1, 2, 3]
        ys = [4, 5]

    -- LADO IZQUIERDO

        unzip (zip [1, 2, 3] [4, 5])
    =                                           (zip.3)
        unzip ((1, 4) : zip [2, 3] [5])
    =                                           (zip.3)
        unzip ((1, 4) : (2, 5) : zip [3] [])
    =                                           (zip.2)
        unzip ((1, 4) : (2, 5) : [])
    =                                           (unzip.2)
        merge 1 4 (unzip ((2, 5) : []))
    =                                           (unzip.2)
        merge 1 4 (merge 2 5 (unzip []))
    =                                           (unzip.1)
        merge 1 4 (merge 2 5 ([], []))
    =                                           (merge.1)
        merge 1 4 ([2], [5])
    =                                           (merge.1)
        ([1, 2], [4, 5])

    -- LADO DERECHO

        ([1, 2, 3], [4, 5])

    Con este contraejemplo, queda evidenciado que ambos lados llegan a conclusiones distintas, y por ende,
    es inválida la propiedad. No vale para todo xs y para todo ys.


#############################################################################################################################

## SECCIÓN 2


#############################################################################################################################

## SECCIÓN 3

> Ejercicio 1:

data ExpA = Cte Int
          | Suma ExpA ExpA 
          | Prod ExpA ExpA
    deriving Show

-- 1.A.I

evalExpA :: ExpA -> Int
evalExpA (Cte n)      = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2

-- 1.A.II

simplificarExpA :: ExpA -> ExpA
simplificarExpA (Cte n)      = Cte n
simplificarExpA (Suma e1 e2) = simplificarSuma (simplificarExpA e1) (simplificarExpA e2)
simplificarExpA (Prod e1 e2) = simplificarProd (simplificarExpA e1) (simplificarExpA e2)

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Cte 0) e2      = e2
simplificarSuma e1      (Cte 0) = e1
simplificarSuma e1      e1      = Suma e1 e2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Cte 0) e2      = Cte 0
simplificarProd e1      (Cte 0) = Cte 0
simplificarProd (Cte 1) e2      = e2
simplificarProd e1      (Cte 1) = e1
simplificarProd e1      e1      = Prod e1 e2

-- 1.A.III

cantidadDeSumaCero :: ExpA -> Int
cantidadDeSumaCero (Cte n)      = 0
cantidadDeSumaCero (Suma e1 e2) = unoSi (esSumaCero e1 e2) + (cantidadDeSumaCero e1) (cantidadDeSumaCero e2)
cantidadDeSumaCero (Prod e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2

esSumaCero :: ExpA -> ExpA -> Bool
esSumaCero (Cte 0) _       = True
esSumaCero _       (Cte 0) = True
esSumaCero _       _       = False


-- 1.B.I

evalExpA . simplificarExpA = evalExpA 


-- 1.B.II

cantidadSumaCero . simplificarExpA = const 0



> Ejercicio 2:

data ExpS = CteS N
          | SumS ExpS ExpS
          | ProdS ExpS ExpS
    deriving Show

-- 2.A.I

evalES :: ExpS -> Int
evalES (CteS n)      =
evalES (SumS e1 e2)  = 
evalES (ProdS e1 e2) =

-- 2.A.II

es2ExpA :: ExpS -> ExpA
es2ExpA (CteS n)      = 
es2ExpA (SumS e1 e2)  =
es2ExpA (ProdS e1 e2) =

-- 2.A.III

expA2es :: ExpA -> ExpS
expA2es (CteS n)      =
expA2es (SumS e1 e2)  =
expA2es (ProdS e1 e2) =


-- 2.B.I

evalExpA . es2ExpA = evalES 


-- 2.B.II

evalES . expA2es = evalExpA 


-- 2.B.III

es2ExpA . expA2es = id 


-- 2.B.IV

expA2es . es2ExpA = id