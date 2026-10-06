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

> Ejercicio 1:

data N = Z | S N
    deriving Show

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
prodN Z     m = Z
prodN (S n) m = addN m (prodN n m)

-- 1.A.IV

int2N :: Int -> N
int2N 0 = Z
int2N n = S (int2N (n-1))


-- 1.B.I

¿Para todo n1. para todo n2. evalN (addN n1 n2) = evalN n1 + evalN n2?

Demostración:
    Sea m1 :: N, m2 :: N. Por principio de inducción en la estructura
    de m1 es equivalente demostrar:

    Caso base (m1 = Z):
        ¿evalN (addN Z m2) = evalN Z + evalN m2?

    Caso inductivo (m1 = (S n)):
        Hipotesis inductiva:
            ¡evalN (addN n m2) = evalN n + evalN m2!

        Tesis inductiva:
            ¿evalN (addN (S n) m2) = evalN (S n) + evalN m2?

    Demostración caso base:
        ¿evalN (addN Z m2) = evalN Z + evalN m2?

    -- LADO IZQUIERDO:

        evalN (addN Z m2)
    =                               (addN.1)
        evalN m2

    -- LADO DERECHO:

        evalN Z + evalN m2
    =                               (evalN.1)
        0 + evalN m2
    =                               (aritmética)
        evalN m2

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿evalN (addN (S n) m2) = evalN (S n) + evalN m2?

        ¡evalN (addN n m2) = evalN n + evalN m2!

    -- LADO IZQUIERDO:

        evalN (addN (S n) m2)
    =                               (addN.2)
        evalN (S (addN n m2))
    =                               (evalN.2)
        1 + evalN (addN n m2)
    =                               (HI)
        1 + evalN n + evalN m2

    -- LADO DERECHO:

        evalN (S n) + evalN m2
    =                               (evalN.2)
        1 + evalN n + evalN m2

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 1.B.I

¿Para todo n1. para todo n2. evalN (prodN n1 n2) = evalN n1 * evalN n2?

Demostración:
    ...


-- 1.B.I

¿int2N . evalN = id?

Demostración:
    ...


-- 1.B.I

¿evalN . int2N = id?

Demostración:
    ...




















> Ejercicio 2:




> Ejercicio 3:




> Ejercicio 4:

type NDec = [DigDec]

data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
    deriving Show

-- 4.A.I

evalND :: NDec -> Int
evalND []       = 0
evalND (nd:nds) = ddAsInt nd + (10 * evalND nds) ???

ddAsInt :: DigDec -> Int ???
ddAsInt D0 = 0
ddAsInt D1 = 1
ddAsInt D2 = 2
ddAsInt D3 = 3
ddAsInt D4 = 4
ddAsInt D5 = 5
ddAsInt D6 = 6
ddAsInt D7 = 7
ddAsInt D8 = 8
ddAsInt D9 = 9

-- 4.A.II

normalizarND :: NDec -> NDec
normalizarND []       =
normalizarND (nd:nds) =

-- 4.A.III

succNDec :: NDec -> NDec
succNDec []       =
succNDec (nd:nds) =

-- 4.A.IV

addNDec :: NDec -> NDec -> NDec
addNDec []       md =
addNDec (nd:nds) md =

-- 4.A.V

nd2nb :: NDec -> NBin
nd2nb []       =
nd2nb (nd:nds) =

-- 4.A.VI

nb2nd :: NBin -> NDec
nb2nd []       =
nb2nd (nb:nbs) =


-- 4.B.I

evalNDec . succNDec = (+1) . evalNDec


-- 4.B.II

¿para todo n1. para todo n2. evalNDec (addNDec n1 n2) = evalNDec n1 + evalNDec n2?


-- 4.B.III

nd2nb . nb2nd = normalizarNB


-- 4.B.IV

nb2nd . nd2nb = id


> Ejercicio 5:

    > Representación en N:
        - 17: (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))))))))
        - 42: (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
              (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))))))))))))))))))))))))))))))

    > Representación en NU:
        - 17: [(), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()]
        - 42: [(), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (),
               (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (),
               (), (), (), (), (), (), (), ()]

    > Representación en NBin:
        - 17: [I, O, O, O, I]
        - 42: [O, I, O, I, O, I]

    > Representación en NDec:
        - 17: [D7, D1]
        - 42: [D2, D4]


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
cantidadDeSumaCero (Suma e1 e2) = unoSi (esSumaCero e1 e2) + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
cantidadDeSumaCero (Prod e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2

esSumaCero :: ExpA -> ExpA -> Bool
esSumaCero (Cte 0) _       = True
esSumaCero _       (Cte 0) = True
esSumaCero _       _       = False


-- 1.B.I

¿evalExpA . simplificarExpA = evalExpA?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que:
    ¿para todo e. (evalExpA . simplificarExpA) e = evalExpA e?

    Por definición de (.) es equivalente demostrar que:
    ¿para todo e. evalExpA (simplificarExpA e) = evalExpA e?

    Sea e' :: ExpA. Por principio de inducción en la estructura
    de e' es equivalente demostrar:

    Caso base (e' = Cte n):
        ¿evalExpA (simplificarExpA (Cte n)) = evalExpA (Cte n)?

    Caso inductivo 1 (e' = Suma e1 e2):
        Hipotesis inductiva 1.1:
            ¡evalExpA (simplificarExpA e1) = evalExpA e1!

        Hipotesis inductiva 1.2:
            ¡evalExpA (simplificarExpA e2) = evalExpA e2!

        Tesis inductiva 1:
            ¿evalExpA (simplificarExpA (Suma e1 e2)) = evalExpA (Suma e1 e2)?

    Caso inductivo 2 (e' = Prod e1 e2):
        Hipotesis inductiva 2.1:
            ¡evalExpA (simplificarExpA e1) = evalExpA e1!

        Hipotesis inductiva 2.2:
            ¡evalExpA (simplificarExpA e2) = evalExpA e2!

        Tesis inductiva 2:
            ¿evalExpA (simplificarExpA (Prod e1 e2)) = evalExpA (Prod e1 e2)?

    Demostración caso base:
        ¿evalExpA (simplificarExpA (Cte n)) = evalExpA (Cte n)?

    -- LADO IZQUIERDO:

        evalExpA (simplificarExpA (Cte n))
    =                                           (simplificarExpA.1)
        evalExpA (Cte n)

    -- LADO DERECHO:

        evalExpA (Cte n)

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 1:
        ¿evalExpA (simplificarExpA (Suma e1 e2)) = evalExpA (Suma e1 e2)?

    -- LADO IZQUIERDO:

        evalExpA (simplificarExpA (Suma e1 e2))
    =                                                                           (simplificarExpA.2)
        evalExpA (simplificarSuma (simplificarExpA e1) (simplificarExpA e2))
    =                                                                           (Lema EvalSimpSuma)
        evalExpA (simplificarExpA e1) + evalExpA (simplificarExpA e2)

    -- LADO DERECHO:

        evalExpA (Suma e1 e2)
    =                                                                           (evalExpA.2)
        evalExpA e1 + evalExpA e2
    =                                                                           (HI 1.1)
        evalExpA (simplificarExpA e1) + evalExpA e2
    =                                                                           (HI 1.2)
        evalExpA (simplificarExpA e1) + evalExpA (simplificarExpA e2)

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 2:
        ¿evalExpA (simplificarExpA (Prod e1 e2)) = evalExpA (Prod e1 e2)?

    -- LADO IZQUIERDO:

        evalExpA (simplificarExpA (Prod e1 e2))
    =                                                                           (simplificarExpA.3)
        evalExpA (simplificarProd (simplificarExpA e1) (simplificarExpA e2))
    =                                                                           (Lema EvalSimpProd)
        evalExpA (simplificarExpA e1) * evalExpA (simplificarExpA e2)

    -- LADO DERECHO:

        evalExpA (Prod e1 e2)
    =                                                                           (evalExpA.2)
        evalExpA e1 * evalExpA e2
    =                                                                           (HI 2.1)
        evalExpA (simplificarExpA e1) * evalExpA e2
    =                                                                           (HI 2.2)
        evalExpA (simplificarExpA e1) * evalExpA (simplificarExpA e2)

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema EvalSimpSuma: ¿para todo e1. para todo e2. evalExpA (simplificarSuma e1 e2) = evalExpA e1 + evalExpA e2?

    Demostración:
        Sea e' :: ExpA, sea e'' :: ExpA. Se verá por casos que:
        ¿evalExpA (simplificarSuma e' e'') = evalExpA e' + evalExpA e''?

        Caso 1 (e' = (Cte 0)):

        -- LADO IZQUIERDO:
        
            evalExpA (simplificarSuma (Cte 0) e'')
        =                                               (simplificarSuma.1)
            evalExpA e''

        -- LADO DERECHO:
        
            evalExpA (Cte 0) + evalExpA e''
        =                                               (evalExpA.1)
            0 + evalExpA e''
        =                                               (aritmética)
            evalExpA e''

        -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 2 (e'' = (Cte 0)):

        -- LADO IZQUIERDO:

            evalExpA (simplificarSuma e' (Cte 0))
        =                                               (simplificarSuma.2)
            evalExpA e'

        -- LADO DERECHO:
        
            evalExpA e' + evalExpA (Cte 0)
        =                                               (evalExpA.1)
            evalExpA e' + 0 
        =                                               (aritmética)
            evalExpA e'

        -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 3 (e' /= (Cte 0), e'' /= (Cte 0)):

        -- LADO IZQUIERDO:

            evalExpA (simplificarSuma e' e'')
        =                                               (simplificarSuma.3)
            evalExpA (Suma e' e'')
        =                                               (evalExpA.2)
            evalExpA e' + evalExpA e''

        -- LADO DERECHO:

            evalExpA e' + evalExpA e''

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema EvalSimpProd: ¿para todo e1. para todo e2. evalExpA (simplificarProd e1 e2) = evalExpA e1 * evalExpA e2?

    Demostración:
        Sea e' :: ExpA, sea e'' :: ExpA. Se verá que:
        ¿evalExpA (simplificarProd e' e'') = evalExpA e' * evalExpA e''?

        Caso 1 (e' = (Cte 0)):

        -- LADO IZQUIERDO:
        
            evalExpA (simplificarProd (Cte 0) e'')
        =                                               (simplificarProd.1)
            evalExpA (Cte 0)
        =                                               (evalExpA.1)
            0

        -- LADO DERECHO:
        
            evalExpA (Cte 0) * evalExpA e''
        =                                               (evalExpA.1)
            0 * evalExpA e''
        =                                               (aritmética)
            0

        -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 2 (e'' = (Cte 0)):

        -- LADO IZQUIERDO:

            evalExpA (simplificarProd e' (Cte 0))
        =                                               (simplificarProd.2)
            evalExpA (Cte 0)
        =                                               (evalExpA.1)
            0

        -- LADO DERECHO:
        
            evalExpA e' * evalExpA (Cte 0)
        =                                               (evalExpA.1)
            evalExpA e' * 0 
        =                                               (aritmética)
            0

        -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 3 (e' = (Cte 1)):

        -- LADO IZQUIERDO:
        
            evalExpA (simplificarProd (Cte 1) e'')
        =                                               (simplificarProd.3)
            evalExpA e''

        -- LADO DERECHO:
        
            evalExpA (Cte 1) * evalExpA e''
        =                                               (evalExpA.1)
            1 * evalExpA e''
        =                                               (aritmética)
            evalExpA e''

        -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 4 (e'' = (Cte 1)):

        -- LADO IZQUIERDO:

            evalExpA (simplificarProd e' (Cte 1))
        =                                               (simplificarProd.4)
            evalExpA e'

        -- LADO DERECHO:
        
            evalExpA e' * evalExpA (Cte 1)
        =                                               (evalExpA.1)
            evalExpA e' * 1
        =                                               (aritmética)
            evalExpA e'

        -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 5 (e' /= (Cte 0), e'' /= (Cte 0), e' /= (Cte 1), e'' /= (Cte 1)):

        -- LADO IZQUIERDO:

            evalExpA (simplificarProd e' e'')
        =                                               (simplificarProd.5)
            evalExpA (Prod e' e'')
        =                                               (evalExpA.3)
            evalExpA e' * evalExpA e''

        -- LADO DERECHO:

            evalExpA e' * evalExpA e''

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 1.B.II

¿cantidadSumaCero . simplificarExpA = const 0?

Demostración:
    Por principio de extensionalidad es equivalente demostrar que:
    ¿para todo e. (cantidadDeSumaCero . simplificarExpA) e = const 0 e?

    Por definición (.), y por definición const, es equivalente demostrar que:
    ¿para todo e. cantidadDeSumaCero (simplificarExpA e) = 0?
    
    Sea e' :: ExpA. Por principio de inducción en la estructura
    de e' es equivalente demostrar:

    Caso base (e' = Cte n):
        ¿cantidadDeSumaCero (simplificarExpA (Cte n)) = 0?

    Caso inductivo 1 (e' = Suma e1 e2):
        Hipotesis inductiva 1.1:
            ¡cantidadDeSumaCero (simplificarExpA e1) = 0!

        Hipotesis inductiva 1.2:
            ¡cantidadDeSumaCero (simplificarExpA e2) = 0!

        Tesis inductiva 1:
            ¿cantidadDeSumaCero (simplificarExpA (Suma e1 e2)) = 0?

    Caso inductivo 2 (e' = Prod e1 e2):
        Hipotesis inductiva 2.1:
            ¡cantidadDeSumaCero (simplificarExpA e1) = 0!

        Hipotesis inductiva 2.2:
            ¡cantidadDeSumaCero (simplificarExpA e2) = 0!

        Tesis inductiva 2:
            ¿cantidadDeSumaCero (simplificarExpA (Prod e1 e2)) = 0?

    Demostración caso base:
        ¿cantidadDeSumaCero (simplificarExpA (Cte n)) = 0?

    -- LADO IZQUIERDO:

        cantidadDeSumaCero (simplificarExpA (Cte n))
    =                                                       (simplificarExpA.1)
        cantidadDeSumaCero (Cte n)
    =                                                       (cantidadDeSumaCero.1)
        0

    -- LADO DERECHO:

        0
    
    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 1:
        ¿cantidadDeSumaCero (simplificarExpA (Suma e1 e2)) = 0?

    -- LADO IZQUIERDO:

        cantidadDeSumaCero (simplificarExpA (Suma e1 e2))
    =                                                                                       (simplificarExpA.2)
        cantidadDeSumaCero (simplificarSuma (simplificarExpA e1) (simplificarExpA e2))
    =                                                                                       (Lema SimplSumaCantCero)
        cantidadDeSumaCero (simplificarExpA e1) + cantidadDeSumaCero (simplificarExpA e2)
    =                                                                                       (HI 1.1)
        0 + cantidadDeSumaCero (simplificarExpA e2)
    =                                                                                       (HI 1.2)
        0 + 0
    =                                                                                       (aritmética)
        0

    -- LADO DERECHO:

        0

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 2:
        ¿cantidadDeSumaCero (simplificarExpA (Prod e1 e2)) = 0?

    -- LADO IZQUIERDO:

        cantidadDeSumaCero (simplificarExpA (Prod e1 e2))
    =                                                                                       (simplificarExpA.3)
        cantidadDeSumaCero (simplificarProd (simplificarExpA e1) (simplificarExpA e2))
    =                                                                                       (Lema SimplProdCantCero, garantizado por HI 2.1 y HI 2.2)
        cantidadDeSumaCero (simplificarExpA e1) + cantidadDeSumaCero (simplificarExpA e2)
    =                                                                                       (HI 2.1)
        0 + cantidadDeSumaCero (simplificarExpA e2)
    =                                                                                       (HI 2.2)
        0 + 0
    =                                                                                       (aritmética)
        0

    -- LADO DERECHO:

        0
    
    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema SimplSumaCantCero: ¿para todo e'. para todo e''. cantidadDeSumaCero (simplificarSuma e' e'') = cantidadDeSumaCero e' + cantidadDeSumaCero e''?

    Demostración:
        Sea e1 :: ExpA, sea e2 :: ExpA. Se verá por casos que:
        ¿cantidadDeSumaCero (simplificarSuma e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2?

    Caso 1 (e1 = (Cte 0)):

    -- LADO IZQUIERDO:

        cantidadDeSumaCero (simplificarSuma (Cte 0) e2)
    =                                                               (simplificarSuma.1)
        cantidadDeSumaCero e2

    -- LADO DERECHO:

        cantidadDeSumaCero (Cte 0) + cantidadDeSumaCero e2
    =                                                               (cantidadDeSumaCero.1)
        0 + cantidadDeSumaCero e2
    =                                                               (aritmética)
        cantidadDeSumaCero e2

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Caso 2 (e2 = (Cte 0)):

    -- LADO IZQUIERDO:

        cantidadDeSumaCero (simplificarSuma e1 (Cte 0))
    =                                                               (simplificarSuma.2)
        cantidadDeSumaCero e1

    -- LADO DERECHO:

        cantidadDeSumaCero e1 + cantidadDeSumaCero (Cte 0)
    =                                                               (cantidadDeSumaCero.1)
        cantidadDeSumaCero e1 + 0
    =                                                               (aritmética)
        cantidadDeSumaCero e1

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Caso 3 (e1 /= (Cte 0), e2 /= (Cte 0)):

    -- LADO IZQUIERDO:

        cantidadDeSumaCero (simplificarSuma e1 e2)
    =                                                                                   (simplificarSuma.3)
        unoSi (esSumaCero e1 e2) + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
    =                                                                                   (esSumaCero.3)
        unoSi False + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
    =                                                                                   (unoSi.2)
        0 + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
    =                                                                                   (aritmética)
        cantidadDeSumaCero e1 + cantidadDeSumaCero e2

    -- LADO DERECHO:

        cantidadDeSumaCero e1 + cantidadDeSumaCero e2

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


    Lema SimplProdCantCero: Si cantidadDeSumaCero e' = 0 y cantidadDeSumaCero e'' = 0, entonces...
    ¿cantidadDeSumaCero (simplificarProd e' e'') = cantidadDeSumaCero e' + cantidadDeSumaCero e''?

    Demostración:
            Sea e1 :: ExpA, sea e2 :: ExpA. Se verá por casos que:
            ¿cantidadDeSumaCero (simplificarProd e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2?

    Caso 1 (e1 = (Cte 0)):

    -- LADO IZQUIERDO:

        cantidadDeSumaCero (simplificarProd (Cte 0) e2)
    =                                                               (simplificarProd.1)
        cantidadDeSumaCero (Cte 0)
    =                                                               (cantidadDeSumaCero.1)
        0

    -- LADO DERECHO:

        cantidadDeSumaCero (Cte 0) + cantidadDeSumaCero e2
    =                                                               (cantidadDeSumaCero.1)
        0 + cantidadDeSumaCero e2
    =                                                               (aritmética)
        cantidadDeSumaCero e2
    =                                                               (Hipotesis del Lema)
        0

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Caso 2 (e2 = (Cte 0)):

    -- LADO IZQUIERDO:

        cantidadDeSumaCero (simplificarProd e1 (Cte 0))
    =                                                               (simplificarProd.2)
        cantidadDeSumaCero (Cte 0)
    =                                                               (cantidadDeSumaCero.1)
        0

    -- LADO DERECHO:

        cantidadDeSumaCero e1 + cantidadDeSumaCero (Cte 0)
    =                                                               (cantidadDeSumaCero.1)
        cantidadDeSumaCero e1 + 0
    =                                                               (aritmética)
        cantidadDeSumaCero e1
    =                                                               (Hipotesis del Lema)
        0

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Caso 3 (e1 = (Cte 1)):

    -- LADO IZQUIERDO:

        cantidadDeSumaCero (simplificarProd (Cte 1) e2)
    =                                                               (simplificarProd.3)
        cantidadDeSumaCero e2

    -- LADO DERECHO:

        cantidadDeSumaCero (Cte 1) + cantidadDeSumaCero e2
    =                                                               (cantidadDeSumaCero.1)
        0 + cantidadDeSumaCero e2
    =                                                               (aritmética)
        cantidadDeSumaCero e2

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Caso 4 (e2 = (Cte 1)):

    -- LADO IZQUIERDO:

        cantidadDeSumaCero (simplificarProd e1 (Cte 1))
    =                                                               (simplificarProd.4)
        cantidadDeSumaCero e1

    -- LADO DERECHO:

        cantidadDeSumaCero e1 + cantidadDeSumaCero (Cte 1)
    =                                                               (cantidadDeSumaCero.1)
        cantidadDeSumaCero e1 + 0
    =                                                               (aritmética)
        cantidadDeSumaCero e1

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Caso 5 (e1 /= (Cte 0), e1 /= (Cte 1), e2 /= (Cte 0), e2 /= (Cte 1)):

    -- LADO IZQUIERDO:

        cantidadDeSumaCero (simplificarProd e1 e2)
    =                                                                                   (simplificarProd.5)
        cantidadDeSumaCero (Prod e1 e2)
    =                                                                                   (cantidadDeSumaCero.3)
        cantidadDeSumaCero e1 + cantidadDeSumaCero e2

    -- LADO DERECHO:

        cantidadDeSumaCero e1 + cantidadDeSumaCero e2

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


> Ejercicio 2:

data ExpS = CteS N
          | SumS ExpS ExpS
          | ProdS ExpS ExpS
    deriving Show

data N = Z | S N
    deriving Show

-- 2.A.I

evalES :: ExpS -> Int
evalES (CteS nz)     = evalN nz
evalES (SumS e1 e2)  = evalES e1 + evalES e2
evalES (ProdS e1 e2) = evalES e1 * evalES e2

-- 2.A.II

es2ExpA :: ExpS -> ExpA
es2ExpA (CteS nz)     = Cte (evalN nz)
es2ExpA (SumS e1 e2)  = Suma (es2ExpA e1) (es2ExpA e2)
es2ExpA (ProdS e1 e2) = Prod (es2ExpA e1) (es2ExpA e2)

-- 2.A.III

expA2es :: ExpA -> ExpS
expA2es (Cte n)      = CteS (int2N n)
expA2es (Suma e1 e2) = SumS (expA2es e1) (expA2es e2)
expA2es (Prod e1 e2) = ProdS (expA2es e1) (expA2es e2)


-- 2.B.I

¿evalExpA . es2ExpA = evalES? 

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que:
    ¿para todo es. (evalExpA . es2ExpA) es = evalES es?

    Por definición de (.) es equivalente demostrar que:
    ¿para todo es. evalExpA (es2ExpA es) = evalES es?

    Sea es1 :: ExpS. Por principio de inducción en la estructura
    de es1 es equivalente demostrar:

    Caso base (es1 = CteS nz):
        ¿evalExpA (es2ExpA (CteS nz)) = evalES (CteS nz)?

    Caso inductivo 1 (es1 = SumS e1 e2):
        Hipotesis inductiva 1.1:
            ¡evalExpA (es2ExpA e1) = evalES e1!

        Hipotesis inductiva 1.2:
            ¡evalExpA (es2ExpA e2) = evalES e2!

        Tesis inductiva 1:
            ¿evalExpA (es2ExpA (SumS e1 e2)) = evalES (SumS e1 e2)?

    Caso inductivo 2 (es1 = ProdS e1 e2):
        Hipotesis inductiva 2.1:
            ¡evalExpA (es2ExpA e1) = evalES e1!

        Hipotesis inductiva 2.2:
            ¡evalExpA (es2ExpA e2) = evalES e2!

        Tesis inductiva 2:
            ¿evalExpA (es2ExpA (ProdS e1 e2)) = evalES (ProdS e1 e2)?

    Demostración caso base:
        ¿evalExpA (es2ExpA (CteS nz)) = evalES (CteS nz)?

    -- LADO IZQUIERDO:

        evalExpA (es2ExpA (CteS nz))
    =                                                   (es2ExpA.1)
        evalExpA (Cte (evalN nz))
    =                                                   (evalExpA.1)
        (evalN nz)

    -- LADO DERECHO:

        evalES (CteS nz)
    =                                                   (evalES.1)
        (evalN nz)

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 1:
        ¿evalExpA (es2ExpA (SumS e1 e2)) = evalES (SumS e1 e2)?

    -- LADO IZQUIERDO:

        evalExpA (es2ExpA (SumS e1 e2))
    =                                                   (es2ExpA.2)
        evalExpA (Suma (es2ExpA e1) (es2ExpA e2))
    =                                                   (evalExpA.2)
        evalExpA (es2ExpA e1) + evalExpA (es2ExpA e2)
    =                                                   (HI 1.1)
        evalES e1 + evalExpA (es2ExpA e2)
    =                                                   (HI 1.2)
        evalES e1 + evalES e2 

    -- LADO DERECHO:

        evalES (SumS e1 e2)
    =                                                   (evalES.2)
        evalES e1 + evalES e2

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 2:
        ¿evalExpA (es2ExpA (ProdS e1 e2)) = evalES (ProdS e1 e2)?

    -- LADO IZQUIERDO:

        evalExpA (es2ExpA (ProdS e1 e2))
    =                                                   (es2ExpA.3)
        evalExpA (Prod (es2ExpA e1) (es2ExpA e2))
    =                                                   (evalExpA.3)
        evalExpA (es2ExpA e1) * evalExpA (es2ExpA e2)
    =                                                   (HI 2.1)
        evalES e1 * evalExpA (es2ExpA e2)
    =                                                   (HI 2.2)
        evalES e1 * evalES e2

    -- LADO DERECHO:

        evalES (ProdS e1 e2)
    =                                                   (evalES.3)
        evalES e1 * evalES e2

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.B.II

¿evalES . expA2es = evalExpA? 

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que:
    ¿para todo ea. (evalES . expA2es) ea = evalExpA ea?

    Por definición de (.) es equivalente demostrar que:
    ¿para todo ea. evalES (expA2es ea) = evalExpA ea?

    Sea ea1 :: ExpA. Por principio de inducción en la estructura
    de ea1 es equivalente demostrar:

    Caso base (ea1 = Cte n):
        ¿evalES (expA2es (Cte n)) = evalExpA (Cte n)?

    Caso inductivo 1 (ea1 = Suma e1 e2):
        Hipotesis inductiva 1.1:
            ¡evalES (expA2es e1) = evalExpA e1!

        Hipotesis inductiva 1.2:
            ¡evalES (expA2es e2) = evalExpA e2!

        Tesis inductiva 1:
            ¿evalES (expA2es (Suma e1 e2)) = evalExpA (Suma e1 e2)?

    Caso inductivo 2 (ea1 = Prod e1 e2):
        Hipotesis inductiva 2.1:
            ¡evalES (expA2es e1) = evalExpA e1!

        Hipotesis inductiva 2.2:
            ¡evalES (expA2es e2) = evalExpA e2!

        Tesis inductiva 2:
            ¿evalES (expA2es (Prod e1 e2)) = evalExpA (Prod e1 e2)?

    Demostración caso base:
        ¿evalES (expA2es (Cte n)) = evalExpA (Cte n)?

    -- LADO IZQUIERDO:

        evalES (expA2es (Cte n))
    =                                                   (expA2es.1)
        evalES (CteS (int2N n))
    =                                                   (evalES.1)
        evalN (int2N n)
    =                                                   (Demostración S2.1.B.IV)
        id n
    =                                                   (id, x <- n)
        n

    -- LADO DERECHO:

        evalExpA (Cte n)
    =                                                   (evalExpA.1)
        n

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 1:
        ¿evalES (expA2es (Suma e1 e2)) = evalExpA (Suma e1 e2)?

    -- LADO IZQUIERDO:

        evalES (expA2es (Suma e1 e2))
    =                                                   (expA2es.2)
        evalES (SumS (expA2es e1) (expA2es e2))
    =                                                   (evalES.2)
        evalES (expA2es e1) + evalES (expA2es e1)
    =                                                   (HI 1.1)
        evalExpA e1 + evalES (expA2es e2)
    =                                                   (HI 1.2)
        evalExpA e1 + evalExpA e2 

    -- LADO DERECHO:

        evalExpA (Suma e1 e2)
    =                                                   (evalExpA.2)
        evalExpA e1 + evalExpA e2

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 2:
        ¿evalES (expA2es (Prod e1 e2)) = evalExpA (Prod e1 e2)?

    -- LADO IZQUIERDO:

        evalES (expA2es (Prod e1 e2))
    =                                                   (expA2es.3)
        evalES (ProdS (expA2es e1) (expA2es e2))
    =                                                   (evalExpA.3)
        evalES (expA2es e1) * evalES (expA2es e1)
    =                                                   (HI 2.1)
        evalExpA e1 * evalES (expA2es e2)
    =                                                   (HI 2.2)
        evalExpA e1 * evalExpA e2

    -- LADO DERECHO:

        evalExpA (Prod e1 e2)
    =                                                   (evalExpA.3)
        evalExpA e1 * evalExpA e2

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.B.III

¿es2ExpA . expA2es = id? 

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que:
    ¿para todo ea. (es2ExpA . expA2es) ea = id ea?

    Por definición de (.) y id es equivalente demostrar que:
    ¿para todo ea. es2ExpA (expA2es ea) = ea?

    Sea ea1 :: ExpA. Por principio de inducción en la estructura
    de ea1 es equivalente demostrar:

    Caso base (ea1 = Cte n):
        ¿es2ExpA (expA2es (Cte n)) = Cte n?

    Caso inductivo 1 (ea1 = Suma e1 e2):
        Hipotesis inductiva 1.1:
            ¡es2ExpA (expA2es e1) = e1!

        Hipotesis inductiva 1.2:
            ¡es2ExpA (expA2es e2) = e2!

        Tesis inductiva 1:
            ¿es2ExpA (expA2es (Suma e1 e2)) = Suma e1 e2?

    Caso inductivo 2 (ea1 = Prod e1 e2):
        Hipotesis inductiva 2.1:
            ¡es2ExpA (expA2es e1) = e1!

        Hipotesis inductiva 2.2:
            ¡es2ExpA (expA2es e2) = e2!

        Tesis inductiva 2:
            ¿es2ExpA (expA2es (Prod e1 e2)) = Prod e1 e2?

    Demostración caso base:
        ¿es2ExpA (expA2es (Cte n)) = Cte n?

    -- LADO IZQUIERDO:

        es2ExpA (expA2es (Cte n))
    =                                       (expA2es.1)
        es2ExpA (CteS (int2N n))
    =                                       (es2ExpA.1)
        Cte (evalN (int2N n))
    =                                       (Demostración S2.1.B.IV)
        Cte (id n)
    =                                       (id, x <- n)
        Cte n

    -- LADO DERECHO:

        Cte n

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 1:
        ¿es2ExpA (expA2es (Suma e1 e2)) = Suma e1 e2?

    -- LADO IZQUIERDO:

        es2ExpA (expA2es (Suma e1 e2))
    =                                                           (expA2es.2)
        es2ExpA (SumS (expA2es e1) (expA2es e2))
    =                                                           (es2ExpA.2)
        Suma (es2ExpA (expA2es e1)) (es2ExpA (expA2es e2))
    =                                                           (HI 1.1)
        Suma e1 (es2ExpA (expA2es e2))
    =                                                           (HI 1.2)
        Suma e1 e2
    
    -- LADO DERECHO:

        Suma e1 e2

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 2:
        ¿es2ExpA (expA2es (Prod e1 e2)) = Prod e1 e2?

    -- LADO IZQUIERDO:

        es2ExpA (expA2es (Prod e1 e2))
    =                                                           (expA2es.3)
        es2ExpA (ProdS (expA2es e1) (expA2es e2))
    =                                                           (es2ExpA.3)
        Prod (es2ExpA (expA2es e1)) (es2ExpA (expA2es e2))
    =                                                           (HI 2.1)
        Prod e1 (es2ExpA (expA2es e2))
    =                                                           (HI 2.2)
        Prod e1 e2

    -- LADO DERECHO:

        Prod e1 e2

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.B.IV

¿expA2es . es2ExpA = id?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que:
    ¿para todo es. (expA2es . es2ExpA) es = id es?

    Por definición de (.) y id es equivalente demostrar que:
    ¿para todo es. expA2es (es2ExpA es) = es?

    Sea es1 :: ExpS. Por principio de inducción en la estructura
    de es1 es equivalente demostrar:

    Caso base (es1 = CteS nz):
        ¿expA2es (es2ExpA (CteS nz)) = CteS nz?

    Caso inductivo 1 (es1 = SumS e1 e2):
        Hipotesis inductiva 1.1:
            ¡expA2es (es2ExpA e1) = e1!

        Hipotesis inductiva 1.2:
            ¡expA2es (es2ExpA e2) = e2!

        Tesis inductiva 1:
            ¿expA2es (es2ExpA (SumS e1 e2)) = SumS e1 e2?

    Caso inductivo 2 (es1 = ProdS e1 e2):
        Hipotesis inductiva 2.1:
            ¡expA2es (es2ExpA e1) = e1!

        Hipotesis inductiva 2.2:
            ¡expA2es (es2ExpA e2) = e2!

        Tesis inductiva 2:
            ¿expA2es (es2ExpA (ProdS e1 e2)) = ProdS e1 e2?

    Demostración caso base:
        ¿expA2es (es2ExpA (CteS nz)) = CteS nz?

    -- LADO IZQUIERDO:

        expA2es (es2ExpA (CteS nz))
    =                                       (es2ExpA.1)
        expA2es (Cte (evalN nz))
    =                                       (expA2es.1)
        CteS (int2N (evalN nz))
    =                                       (Demostración S2.1.B.III)
        CteS (id nz)
    =                                       (id, x <- n)
        CteS nz

    -- LADO DERECHO:

        CteS nz

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 1:
        ¿expA2es (es2ExpA (SumS e1 e2)) = SumS e1 e2?

    -- LADO IZQUIERDO:

        expA2es (es2ExpA (SumS e1 e2))
    =                                                           (es2ExpA.2)
        expA2es (Suma (es2ExpA e1) (es2ExpA e2))
    =                                                           (expA2es.2)
        SumS (expA2es (es2ExpA e1)) (expA2es (es2ExpA e2))
    =                                                           (HI 1.1)
        SumS e1 (expA2es (es2ExpA e2))
    =                                                           (HI 1.2)
        SumS e1 e2
    
    -- LADO DERECHO:

        SumS e1 e2

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 2:
        ¿expA2es (es2ExpA (ProdS e1 e2)) = Prod e1 e2?

    -- LADO IZQUIERDO:

        expA2es (es2ExpA (ProdS e1 e2))
    =                                                           (es2ExpA.3)
        expA2es (Prod (es2ExpA e1) (es2ExpA e2))
    =                                                           (expA2es.2)
        ProdS (expA2es (es2ExpA e1)) (expA2es (es2ExpA e2))
    =                                                           (HI 2.1)
        ProdS e1 (expA2es (es2ExpA e2))
    =                                                           (HI 2.2)
        ProdS e1 e2

    -- LADO DERECHO:

        ProdS e1 e2

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.