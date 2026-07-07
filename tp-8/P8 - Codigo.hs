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
    =                               ((++).1)
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
    =                                   ((++).2)
        length (z : (zs' ++ ws))
    =                                   (length.2)
        1 + length (zs' ++ ws)
    =                                   (HI)
        1 + (length zs' + length ws)

    -- LADO DERECHO

        length (z:zs') + length ws
    =                                   (length.2)
        1 + length zs' + length ws
    =                                   (aritmética)
        1 + (length zs' + length ws)

    -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.B

¿Para todo xs. para todo ys. para todo zs. (xs ++ ys) ++ zs = xs ++ (ys ++ zs)?

Demostración:
    Sea as, bs y cs listas cualquiera (finitas y bien definidas). Por principio de inducción
    en la estructura as es equivalente demostrar:

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
    =                           ((++).1)
        bs ++ cs

    -- LADO DERECHO

        [] ++ (bs ++ cs)
    =                           ((++).1)
        bs ++ cs

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿((a:as') ++ bs) ++ cs = (a:as') ++ (bs ++ cs)?

    -- LADO IZQUIERDO

        ((a:as') ++ bs) ++ cs
    =                               ((++).2)
        (a : (as' ++ bs)) ++ cs
    =                               ((++).2)
        a : ((as' ++ bs) ++ cs)
    =                               (HI)
        a : (as' ++ (bs ++ cs))

    -- LADO DERECHO

        (a:as') ++ (bs ++ cs)
    =                               ((++).2)
        a : (as' ++ (bs ++ cs))

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.C

count (const True) = length

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que 
    ¿Para todo ws. count (const True) ws = length ws?

    Sea xs una lista cualquiera (finita y bien definida). Por principio de inducción
    en la estructura xs es equivalente demostrar:

    Caso base (xs = [])
        ¿count (const True) [] = length []? 

    Caso inductivo (xs = (x:xs'))
        Hipotesis inductiva:
            ¡count (const True) xs' = length xs'!

        Tesis inductiva:
            ¿count (const True) (x:xs') = length (x:xs')?

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
        ¿count (const True) (x:xs') = length (x:xs')?

    -- LADO IZQUIERDO

        count (const True) (x:xs')
    =                                                               (count.2)
        unoSi (const True) x + count (const True) xs'
    =                                                               (unoSi.1)
        if (const True) x then 1 else 0 + count (const True) xs'
    =                                                               (const.1)
        if True then 1 else 0 + count (const True) xs'
    =                                                               (if.then)
        1 + count (const True) xs'
    =                                                               (HI)
        1 + length xs'

    -- LADO DERECHO

        length (x:xs')
    =                                                               (length.2)
        1 + length xs'

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.D

elem = any . (==)

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿Para todo z'. para todo zs. elem z' zs = (any . (==)) z' zs?

    Sea xs una lista cualquiera (finita y bien definida) y x' un elemento del mismo tipo.
    Por principio de inducción en la estructura ws es equivalente demostrar que:

    Caso base (xs = []):
        ¿elem x' [] = (any . (==)) x' []?

    Caso inductivo (xs = (x:xs')):
        Hipotesis inductiva:
            ¡elem x' xs' = (any . (==)) x' xs'!

        Tesis inductiva:
            ¿elem x' (x:xs') = (any . (==)) x' (x:xs')?

    Demostración caso base:
        ¿elem x' [] = (any . (==)) x' []?

    -- LADO IZQUIERDO

        elem x' []
    =                           (elem.1)
        False

    -- LADO DERECHO

        (any . (==)) x' []
    =                           (.)
        any ((==) x') []
    =                           (any.1)
        False

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿elem x' (x:xs') = (any . (==)) x' (x:xs')?

    -- LADO IZQUIERDO

        elem x' (x:xs')
    =                                       (elem.2)
        x' == x || elem x' xs'
    =                                       (HI)
        x' == x || (any . (==)) x' xs'
    =                                       (.)
        x' == x || any ((==) x') xs'

    -- LADO DERECHO

        (any . (==)) x' (x:xs')
    =                                       (.)
        any ((==) x') (x:xs')
    =                                       (any.2)
        ((==) x') x || any ((==) x') xs'
    =                                       ((==))
        x' == x || any ((==) x') xs'

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.E

¿Para todo xs'. any (elem xs') = elem xs' . concat?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿Para todo xs'. para todo xss. any (elem xs') xss = (elem xs' . concat) xss?

    Sea wss una lista cualquiera (finita y bien definida) y ws' un elemento del mismo tipo.
    Por principio de inducción en la estructura wss es equivalente demostrar que:

    Caso base (wss = []):
        ¿any (elem ws') [] = (elem ws' . concat) []?

    Caso inductivo (wss = (ws:wss')):
        Hipotesis inductiva:
            ¡any (elem ws') wss' = (elem ws' . concat) wss'!

        Tesis inductiva:
            ¿any (elem ws') (ws:wss') = (elem ws' . concat) (ws:wss')?

    Demostración caso base:
        ¿any (elem ws') [] = (elem ws' . concat) []?

    -- LADO IZQUIERDO

        any (elem ws') []
    =                           (any.1)
        False

    -- LADO DERECHO

        (elem ws' . concat) []
    =                           (.)
        elem ws' (concat [])
    =                           (concat.1)
        elem ws' []
    =                           (elem.1)
        False

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿any (elem ws') (ws:wss') = (elem ws' . concat) (ws:wss')?

    -- LADO IZQUIERDO

        any (elem ws') (ws:wss')
    =                                           (any.2)
        elem ws' ws || any (elem ws') wss'
    =                                           (HI)
        elem ws' ws || (elem ws' . concat) wss'
    =                                           (.)
        elem ws' ws || elem ws' (concat wss')
    =                                           (Lema ElemAppend)
        elem ws' (ws ++ concat wss')

    -- LADO DERECHO

        (elem ws' . concat) (ws:wss')
    =                                           (.)
        elem ws' (concat (ws:wss'))
    =                                           (concat.2)
        elem ws' (ws ++ concat wss')

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema ElemAppend: ¿para todo ws1. para todo ws2. para todo w'. elem w' ws1 || elem w' ws2 = elem w' (ws1 ++ ws2)? 

    Demostración:
        Sea xs1 y xs2 dos listas cualquiera (finitas y bien definidas) y x' un elemento del mismo tipo.
        Por principio de inducción en la estructura xs1 es equivalente demostrar que:

        Caso base (xs1 = []):
            ¿elem x' [] || elem x' xs2 = elem x' ([] ++ xs2)?

        Caso inductivo (xs1 = (x1:xs1')):
            Hipotesis inductiva:
                ¡elem x' xs1' || elem x' xs2 = elem x' (xs1' ++ xs2)!

            Tesis inductiva:
                ¿elem x' (x1:xs1') || elem x' xs2 = elem x' ((x1:xs1') ++ xs2)?

        Demostración caso base:
            ¿elem x' [] || elem x' xs2 = elem x' ([] ++ xs2)?

        -- LADO IZQUIERDO

            elem x' [] || elem x' xs2
        =                                       (elem.1)
            False || elem x' xs2
        =                                       ((||).2)
            elem x' xs2

        -- LADO DERECHO

            elem x' ([] ++ xs2)
        =                                       ((++).1)
            elem x' xs2
        
            -- Ambos lados llegan a lo mismo, el caso es válido.

        Demostración caso inductivo:
            ¿elem x' (x1:xs1') || elem x' xs2 = elem x' ((x1:xs1') ++ xs2)?

        -- LADO IZQUIERDO

            elem x' (x1:xs1') || elem x' xs2
        =                                               (elem.2)
            x' == x1 || elem x' xs1' || elem x' xs2
        =                                               (HI)
            x' == x1 || elem x' (xs1' ++ xs2)

        -- LADO DERECHO

            elem x' ((x1:xs1') ++ xs2)
        =                                       ((++).2)
            elem x' (x1 : (xs1' ++ xs2))
        =                                       (elem.2)
            x' == x1 || elem x' (xs1' ++ xs2)

            -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.F

¿Para todo xs. para todo ys. subset xs ys = all (flip elem ys) xs?

Demostración:
    Sea ws y zs listas cualquiera (finitas y bien definidas). Por principio de inducción
    en la estructura ws es equivalente demostrar que:

    Caso base (ws = []):
        ¿subset [] zs = all (flip elem zs) []?

    Caso inductivo (ws = (w:ws')):
        Hipotesis inductiva:
            ¡subset ws' zs = all (flip elem zs) ws'!

        Tesis inductiva:
            ¿subset (w:ws') zs = all (flip elem zs) (w:ws')?

    Demostración caso base:
        ¿subset [] zs = all (flip elem zs) []?

    -- LADO IZQUIERDO

        subset [] zs
    =                           (subset.1)
        True

    -- LADO DERECHO

        all (flip elem zs) []
    =                           (all.1)
        True

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿subset (w:ws') zs = all (flip elem zs) (w:ws')?
    
    -- LADO IZQUIERDO

        subset (w:ws') zs
    =                                               (subset.2)
        elem w zs && subset ws' zs
    =                                               (HI)
        elem w zs && all (flip elem zs) ws'

    -- LADO DERECHO

        all (flip elem zs) (w:ws')
    =                                               (all.2)
        (flip elem zs) w && all (flip elem zs) ws'
    =                                               (flip.1)
        elem w zs && all (flip elem zs) ws'

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.G

all null = null . concat

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿Para todo xss. all null xss = (null . concat) xss?

    Sea wss una lista cualquiera (finita y bien definida). Por principio de inducción
    en la estructura wss es equivalente demostrar que:

    Caso base (wss = []):
        ¿all null [] = (null . concat) []?

    Caso inductivo (wss = (ws:wss')):
        Hipotesis inductiva:
            ¡all null wss' = (null . concat) wss'!
        
        Tesis inductiva:
            ¿all null (ws:wss') = (null . concat) (ws:wss')?

    Demostración caso base:
        ¿all null [] = (null . concat) []?

    -- LADO IZQUIERDO

        all null []
    =                       (all.1)
        True

    -- LADO DERECHO

        (null . concat) []
    =                       (.)
        null (concat [])
    =                       (concat.1)
        null []
    =                       (null.1)
        True

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿all null (ws:wss') = (null . concat) (ws:wss')? REVISANDO
        
    -- LADO IZQUIERDO

        all null (ws:wss')
    =                                   (all.2)
        null ws && all null wss'
    =                                   (HI)
        null ws && (null . concat) wss'
    =                                   (.)
        null ws && null (concat wss')
    =                                   (Lema NullConcat)
        null (ws ++ concat wss')

    -- LADO DERECHO

        (null . concat) (ws:wss')
    =                                   (.)
        null (concat (ws:wss'))
    =                                   (concat.2)
        null (ws ++ concat wss')

        -- Ambos lados llegan a lo imsmo, el caso es válido y la propiedad también.

    Lema NullConcat: ¿para todo xs. para todo ys. null xs && null ys = null (xs ++ ys)?

    Demostración:
        Sea ws y zs dos listas cualquiera (finitas y bien definidas). Por principio de inducción 
        en la estructura de ws es equivalente demostrar que:

        Caso base (ws = []):
            ¿null [] && null zs = null ([] ++ zs)?

        Caso inductivo (ws = (w:ws')):
            Hipotesis inductiva:
                ¡null ws' && null zs = null (ws' ++ zs)!

            Tesis inductiva:
                ¿null (w:ws') && null zs = null ((w:ws') ++ zs)?

        Demostración caso base:
            ¿null [] && null zs = null ([] ++ zs)?

        -- LADO IZQUIERDO

            null [] && null zs
        =                           (null.1)
            True && null zs
        =                           ((&&).1)
            null zs

        -- LADO DERECHO

            null ([] ++ zs)
        =                           ((++).1)
            null zs

            -- Ambos lados llegan a lo mismo, el caso es válido.

        Demostración caso inductivo:
            ¿null (w:ws') && null zs = null ((w:ws') ++ zs)?

        -- LADO IZQUIERDO

            null (w:ws') && null zs
        =                                   (null.2)
            False && null zs
        =                                   ((&&).2)
            False

        -- LADO DERECHO

            null ((w:ws') ++ zs)
        =                                   ((++).2)
            null (w : (ws' ++ zs))
        =                                   (null.2)
            False

            -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.H

length = length . reverse

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿Para todo xs. length xs = (length . reverse) xs?

    Sea ws una lista cualquiera (finita y bien definida). Por principio de inducción
    en la estructura ws es equivalente demostrar que:

    Caso base (ws = []):
        ¿length [] = (length . reverse) []?

    Caso inductivo (ws = (w:ws')):
        Hipotesis inductiva:
            ¡length ws' = (length . reverse) ws'!

        Tesis inductiva:
            ¿length (w:ws') = (length . reverse) (w:ws')?

    Demostración caso base:
        ¿length [] = (length . reverse) []?

    -- LADO IZQUIERDO

        length []
    =                           (length.1)
        0

    -- LADO DERECHO

        (length . reverse) []
    =                           (.)
        length (reverse [])
    =                           (reverse.1)
        length []
    =                           (length.1)
        0

        -- Ambos lados llegan a lo mismo, la propiedad es válida.

    Demostración caso inductivo:
        ¿length (w:ws') = (length . reverse) (w:ws')?

    -- LADO IZQUIERDO

        length (w:ws')
    =                                   (length.2)
        1 + length ws'
    =                                   (HI)
        1 + (length . reverse) ws'
    =                                   (.)
        1 + length (reverse ws')


    -- LADO DERECHO

        (length . reverse) (w:ws')
    =                                           (.)
        length (reverse (w:ws'))
    =                                           (reverse.2)
        length (reverse ws' ++ [w])
    =                                           (Propiedad demostrada en S1.2.A)
        length (reverse ws') + length [w]
    =                                           ((:))
        length (reverse ws') + length (w:[])
    =                                           (length.2)
        length (reverse ws') + 1 + length []
    =                                           (length.1)
        length (reverse ws') + 1 + 0
    =                                           (aritmética)
        1 + length (reverse ws')

        -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 2.I

¿Para todo xs. para todo ys. reverse (xs ++ ys) = reverse ys ++ reverse xs?

Demostración:
    Sea ws y zs listas cualquiera (finitas y bien definidas). Por principio de inducción 
    en la estructura ws es equivalente demostrar que:

    Caso base (ws = []):
        ¿reverse ([] ++ zs) = reverse zs ++ reverse []?

    Caso inductivo (ws = (w:ws')):
        Hipotesis inductiva:
            ¡reverse (ws' ++ zs) = reverse zs ++ reverse ws'!

        Tesis inductiva:
            ¿reverse ((w:ws') ++ zs) = reverse zs ++ reverse (w:ws')?

    Demostración caso base:
        ¿reverse ([] ++ zs) = reverse zs ++ reverse []?

    -- LADO IZQUIERDO

        reverse ([] ++ zs)
    =                               ((++).1)
        reverse zs

    -- LADO DERECHO

        reverse zs ++ reverse []
    =                               (reverse.1)
        reverse zs ++ []
    =                               (Lema AppEmpty)
        reverse zs

        -- Ambos lados llegan a lo mismo, este caso es válido.

    Demostración caso inductivo:
        ¿reverse ((w:ws') ++ zs) = reverse zs ++ reverse (w:ws')?
    
    -- LADO IZQUIERDO

        reverse ((w:ws') ++ zs)
    =                                       ((++).2)
        reverse (w : (ws' ++ zs))
    =                                       (reverse.2)
        reverse (ws' ++ zs) ++ [w]

    -- LADO DERECHO

        reverse zs ++ reverse (w:ws')
    =                                       (reverse.2)
        reverse zs ++ reverse ws' ++ [w]
    =                                       (HI)
        reverse (ws' ++ zs) ++ [w]

        -- Ambos lados llegan a lo mismo, este caso es válido y la propiedad también.

    Lema AppEmpty: ¿para todo xs. xs ++ [] = xs?

    Demostración:
        Sea ws una lista cualquiera (finita y bien definida). Por principio de inducción
        en la estructura ws es equivalente demostrar que:

        Caso base (ws = []):
            ¿[] ++ [] = []?

        Caso inductivo (w = (w:ws')):
            Hipotesis inductiva:
                ¡ws' ++ [] = ws'!

            Tesis inductiva:
                ¿(w:ws') ++ [] = (w:ws')?
        
        Demostración caso base:
            ¿[] ++ [] = []?

        -- LADO IZQUIERDO

            [] ++ []
        =               ((++).1)
            []

        -- LADO DERECHO

            []

            -- Ambos lados llegaron a lo mismo, el caso es válido.

        Demostración caso inductivo:
            ¿(w:ws') ++ [] = (w:ws')?

        -- LADO IZQUIERDO

            (w:ws') ++ []
        =                       ((++).2)
            w : (ws' ++ [])
        =                       (HI)
            (w : ws')

        -- LADO DERECHO

            (w:ws')

            -- Ambos lados llegaron a lo mismo, el caso es válido y la propiedad también.


-- 2.J

¿Para todo xs. para todo ys. all p (xs ++ ys) = all p (reverse xs) && all p (reverse ys)?

Demostración:
    Sea ws y zs listas cualquiera (finitas y bien definidas). Por principio de inducción
    en la estructura ws es equivalente a demostrar que:

    Caso base (ws = []):
        ¿all p ([] ++ zs) = all p (reverse []) && all p (reverse zs)?

    Caso inductivo (ws = (w:ws')):
        Hipotesis inductiva:
            ¡all p (ws' ++ zs) = all p (reverse ws') && all p (reverse zs)!

        Tesis inductiva:
            ¿all p ((w:ws') ++ zs) = all p (reverse (w:ws')) && all p (reverse zs)?

    Demostración caso base:
        ¿all p ([] ++ zs) = all p (reverse []) && all p (reverse zs)?

    -- LADO IZQUIERDO

        all p ([] ++ bs)
    =                           ((++).1)
        all p bs

    -- LADO DERECHO

        all p (reverse []) && all p (reverse bs)
    =                                               (reverse.1)
        all p [] && all p (reverse bs)
    =                                               (all.1)
        True && all p (reverse bs)
    =                                               ((&&).1)
        all p (reverse bs)
    =                                               (Lema AllReverse)
        all p bs

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿all p ((w:ws') ++ zs) = all p (reverse (w:ws')) && all p (reverse zs)?

    -- LADO IZQUIERDO

        all p ((w:ws') ++ zs)
    =                                                       ((++).2)
        all p (w : (ws' ++ zs))
    =                                                       (all.2)
        p w && all p (ws' ++ zs)
    =                                                       (HI)
        p w && all p (reverse ws') && all p (reverse zs)
    =                                                       (Lema AllReverse)
        p w && all p ws' && all p (reverse zs)

    -- LADO DERECHO

        all p (reverse (w:ws')) && all p (reverse zs)
    =                                                       (Lema AllReverse)
        all p (w:ws') && all p (reverse zs)
    =                                                       (all.2)
        p w && all p ws' && all p (reverse zs)

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema AllReverse: ¿para todo xs. all p xs = all p (reverse xs)?

    Demostración:
        Sea ws una lista cualquiera (finita y bien definida). Por principio de inducción
        en la estructura ws es equivalente demostrar que:

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
            all p ws' && all p [w]
        =                                       ((:))
            all p ws' && all p (w:[])
        =                                       (all.2)
            all p ws' && p w && all p []
        =                                       (all.1)
            all p ws' && p w && True
        =                                       ((&&).1)
            all p ws' && p w
        =                                       (aritmética)
            p w && all p ws'

            -- Ambos lados legan a lo mismo, el caso es válido y la propiedad también.

    Lema AllDist: ¿para todo xs. para todo ys. all p (xs ++ ys) = all p xs && all p ys?

    Demostración:
        Sea ks y js dos listas cualquiera (finitas y bien definidas). Por principio de inducción
        estructural en ks es equivalente demostrar que:

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

¿Para todo xs. para todo ys. unzip (zip xs ys) = (xs, ys)?  

    Esta propiedad no se cumple para todo xs y para todo ys. Para demostrarlo, propongo el siguiente ejemplo:
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

¿Para todo n'. para todo n''. evalN (addN n' n'') = evalN n' + evalN n''?

Demostración:
    Sea n1 y n2 dos elementos cualquiera de tipo N. Por principio de inducción
    en la estructura n1 es equivalente demostrar que:

    Caso base (n1 = Z):
        ¿evalN (addN Z n2) = evalN Z + evalN n2?

    Caso inductivo (n1 = (S n')):
        Hipotesis inductiva:
            ¡evalN (addN n' m) = evalN n' + evalN m!

        Tesis inductiva:
            ¿evalN (addN (S n') m) = evalN (S n') + evalN m?

    Demostración caso base:
        ¿evalN (addN Z m) = evalN Z + evalN m?

    -- LADO IZQUIERDO

        evalN (addN Z m)
    =                           (addN.1)
        evalN m

    -- LADO DERECHO

        evalN Z + evalN m
    =                           (evalN.1)
        0 + evalN m
    =                           (aritmética)
        evalN m

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿evalN (addN (S n') m) = evalN (S n') + evalN m?

    -- LADO IZQUIERDO

        evalN (addN (S n') m)
    =                               (addN.2)
        evalN (S (addN n' m))
    =                               (evalN.2)
        1 +  evalN (addN n' m)
    =                               (HI)
        1 + (evalN n' + evalN m)
    =                               (aritmética)
        1 + evalN n' + evalN m

    -- LADO DERECHO

        evalN (S n') + evalN m
    =                               (evalN.2)
        (1 + evalN n') + evalN m
    =                               (aritmética)
        1 + evalN n' + evalN m

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 1.B.II

¿Para todo n1. para todo n2. evalN (prodN n1 n2) = evalN n1 * evalN n2?

Demostración:
    Sea n y m dos elementos cualquiera de tipo N. Por principio de inducción en la estructura n
    es equivalente demostrar que:

    Caso base (n = Z):
        ¿evalN (prodN Z m) = evalN Z * evalN m?

    Caso inductivo (n = (S n')):
        Hipotesis inductiva:
            ¡evalN (prodN n' m) = evalN n' * evalN m!

        Tesis inductiva:
            ¿evalN (prodN (S n') m) = evalN (S n') * evalN m?

    Demostración caso base:
        ¿evalN (prodN Z m) = evalN Z * evalN m?

    -- LADO IZQUIERDO

        evalN (prodN Z m)
    =                           (prodN.1)
        evalN Z
    =                           (evalN.1)
        0

    -- LADO DERECHO

        evalN Z * evalN m
    =                           (evalN.1)
        0 * evalN m
    =                           (aritmética)
        0

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿evalN (prodN (S n') m) = evalN (S n') * evalN m?

    -- LADO IZQUIERDO

        evalN (prodN (S n') m)
    =                                   (prodN.2)
        evalN (addN m (prodN n' m))
    =                                   (Lema EvalN)
        evalN m + evalN (prodN n' m)
    =                                   (HI)
        evalN m + (evalN n' * evalN m)

    -- LADO DERECHO

        evalN (S n') * evalN m
    =                                           (evalN.2)
        (1 + evalN n') * evalN m
    =                                           (aritmética)
        (evalN m * 1) + (evalN n' * evalN m)
    =                                           (aritmética)
        evalN m + (evalN n' * evalN m)

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema EvalN: ¿evalN (addN x (prodN y x)) = evalN x + evalN (prodN y x)?

    Demostración:
        Sea a y b dos elementos cualquiera de tipo N. Por principio de inducción en la estructura a
        es equivalente demostrar que:

        Caso base (a = Z):
            ¿evalN (addN Z (prodN b Z)) = evalN Z + evalN (prodN b Z)?

        Caso inductivo (a = (S a')):
            Hipotesis inductiva:
                ¡evalN (addN a' (prodN b a')) = evalN a' + evalN (prodN b a')!

            Tesis inductiva:
                ¿evalN (addN (S a') (prodN b (S a'))) = evalN (S a') + evalN (prodN b (S a'))?

        Demostración caso base:
            ¿evalN (addN Z (prodN b Z)) = evalN Z + evalN (prodN b Z)?

        -- LADO IZQUIERDO

            evalN (addN Z (prodN b Z))
        =                                       (addN.1)
            evalN (prodN b Z)

        -- LADO DERECHO

            evalN Z + evalN (prodN b Z)
        =                                       (evalN.1)
            0 + evalN (prodN b Z)
        =                                       (aritmética)
            evalN (prodN b Z)

            -- Ambos lados llegan a lo mismo, el caso es válido.

        Demostración caso inductivo:
            ¿evalN (addN (S a') (prodN b (S a'))) = evalN (S a') + evalN (prodN b (S a'))?

        -- LADO IZQUIERDO

            evalN (addN (S a') (prodN b (S a')))
        =                                                   (add.2)
            evalN (S (addN a' (prodN b (S a'))))
        =                                                   (evalN.2)
            1 + evalN (addN a' (prodN b (S a')))
        =                                                   (Propiedad demostrada anteriormente en el 1.B.I)
            1 + evalN a' + evalN (prodN b (S a'))

        -- LADO DERECHO

            evalN (S a') + evalN (prodN b (S a'))
        =                                                   (evalN.2)
            1 + evalN a' + evalN (prodN b (S a'))

            -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 1.B.III

int2N . evalN = id

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿Para todo n. int2N . evalN n = id n?

    Sea m un elemento cualquiera de tipo N. Por principio de inducción en la estructura m
    es equivalente demostrar que:

    Caso base (m = Z):
        ¿int2N . evalN Z = id Z?

    Caso inductivo (m = (S m')):
        Hipotesis inductiva:
            ¡int2N . evalN m' = id m'!

        Tesis inductiva:
            ¿int2N . evalN (S m') = id (S m')?

    Demostración caso base:
        ¿int2N . evalN Z = id Z?

    -- LADO IZQUIERDO

        int2N . evalN Z
    =                           (.)
        int2N (evalN Z)
    =                           (evalN.1)
        int2N 0
    =                           (int2N.1)
        Z

    -- LADO DERECHO

        id Z
    =                           (id.1)
        Z

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo.
            ¿int2N . evalN (S m') = id (S m')?

    -- LADO IZQUIERDO

        int2N . evalN (S m')
    =                               (.)
        int2N (evalN (S m'))
    =                               (int2N.2)
        S (int2N ((evalN (S m'))-1))
    =                               (evalN.2)
        S (int2N ((1 + evalN m')-1))
    =                               (aritmética)
        S (int2N (evalN m'))
    =                               (.)
        S (int2N . evalN m')
    =                               (HI)
        S (id m')
    =                               (id.1)
        S (m')
    =                               (aritmética)
        (S m')

    -- LADO DERECHO

        id (S m')
    =                               (id.1)
        (S m')

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 1.B.IV

evalN . int2N = id 

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿Para todo x. evalN . int2N x = id x?

    Sea n un número cualquiera. Por principio de inducción en la estructura n
    es equivalente demostrar que:

    Caso base (n = 0):
        ¿evalN . int2N 0 = id 0?

    Caso inductivo (n = (n-1)+1):
        Hipotesis inductiva:
            ¡evalN . int2N (n-1) = id (n-1)!

        Tesis inductiva:
            ¿evalN . int2N n = id n? -- En realidad es ¿evalN . int2N (n-1)+1 = id (n-1)+1? Pero se simplifica.

    Demostración caso base:
        ¿evalN . int2N 0 = id 0?

    -- LADO IZQUIERDO

        evalN . int2N 0
    =                           (.)
        evalN (int2N 0)
    =                           (int2N.1)
        evalN Z
    =                           (evalN.1)
        0

    -- LADO DERECHO

        id 0
    =                           (id.1)
        0

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿evalN . int2N n = id n?

    -- LADO IZQUIERDO

        evalN . int2N n
    =                               (.)
        evalN (int2N n)
    =                               (int2N.2)
        evalN (S (int2N (n-1)))
    =                               (evalN.2)
        1 + evalN (int2N (n-1))
    =                               (.)
        1 + evalN . int2N (n-1)
    =                               (HI)
        1 + (id (n-1))
    =                               (id.1)
        1 + (n-1)
    =                               (aritmética)
        1

    -- LADO DERECHO

        id n
    =                                   (id.1)
        n

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


> Ejercicio 2:

type NU = [()]                  -- Interpretación alternativa: type NU = [Unit], donde data Unit = Unit

-- 2.A.I

evalNU :: NU -> Int
evalNU []     = 0
evalNU (u:us) = 1 + evalNU us


-- 2.A.II

succNU :: NU -> NU              -- Hay una alternativa donde no es necesaria la recursión estructural.
succNU []     = [()]
succNU (u:us) = u : succNU us


-- 2.A.III

addNU :: NU -> NU -> NU
addNU []     n = n
addNU (u:us) n = u : addNU us n


-- 2.A.IV

nu2n :: NU -> N
nu2n []     = Z
nu2n (_:us) = S (nu2n us)


-- 2.A.V

n2nu :: N -> NU
n2nu Z     = []
n2nu (S n) = () : n2nu n


-- 2.B.I

evalNU . succNU = (+1) . evalNU 

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿Para todo n1. evalNU . succNU n1 = (+1) . evalNU n1?

    Sea n un elemento cualquiera de tipo NU. Por principio de inducción en la estructura n
    es equivalente demostrar que:

    Caso base (n = []):
        ¿evalNU . succNU [] = (+1) . evalNU []?

    Caso inductivo (n = (u:us)):
        Hipotesis inductiva:
            ¡evalNU . succNU us = (+1) . evalNU us!

        Tesis inductiva:
            ¿evalNU . succNU (u:us) = (+1) . evalNU (u:us)?

    Demostración caso base:
        ¿evalNU . succNU [] = (+1) . evalNU []?

    -- LADO IZQUIERDO

        evalNU . succNU []
    =                               (.)
        evalNU (succNU [])
    =                               (succNU.1)
        evalNU [()]
    =                               (evalNU.1)
        1 + evalNU []
    =                               (evalNU.0)
        1 + 0
    =                               (aritmética)
        1

    -- LADO DERECHO

        (+1) . evalNU []
    =                               (.)
        (+1) (evalNU [])
    =                               (evalNU.1)
        (+1) 0
    =                               (+)
        1 + 0
    =                               (aritmética)
        1

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
            ¿evalNU . succNU (u:us) = (+1) . evalNU (u:us)?

    -- LADO IZQUIERDO

        evalNU . succNU (u:us)
    =                                   (.)
        evalNU (succNU (u:us))
    =                                   (succNU.2)          
        evalNU (u : succNU us)
    =                                   (evalNU.2)
        1 + evalNU (succNU us)
    =                                   (.)
        1 + evalNU . succNU us
    =                                   (HI)
        1 + ((+1) . evalNU us)
    =                                   (.)
        1 + ((+1) (evalNU us))
    =                                   (+)
        1 + (1 + (evalNU us))
    =                                   (aritmética)
        1 + 1 + (evalNU us)

    -- LADO DERECHO

        (+1) . evalNU (u:us)
    =                                   (.)
        (+1) (evalNU (u:us))
    =                                   (evalNU.2)
        (+1) (1 + evalNU us)
    =                                   (+)
        1 + (1 + evalNU us)
    =                                   (aritmética)
        1 + 1 + evalNU us

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.B.II

¿Para todo n1. para todo n2. evalNU (addNU n1 n2) = evalNU n1 + evalNU n2?

Demostración:
    Sean n, m dos elementos cualquiera de tipo NU. Por principio de inducción sobre la
    estructura n es equivalente demostrar que:

    Caso base (n = []):
        ¿evalNU (addNU [] m) = evalNU [] + evalNU m?

    Caso inductivo (n = (u:us)):
        Hipotesis inductiva:
            ¡evalNU (addNU us m) = evalNU us + evalNU m!

        Tesis inductiva:
            ¿evalNU (addNU (u:us) m) = evalNU (u:us) + evalNU m?

    Demostración caso base:
        ¿evalNU (addNU [] m) = evalNU [] + evalNU m?

    -- LADO IZQUIERDO

        evalNU (addNU [] m)
    =                               (addNU.1)
        evalNU m

    -- LADO DERECHO

        evalNU [] + evalNU m
    =                               (evalNU.1)
        0 + evalNU m
    =                               (aritmética)
        evalNU m

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿evalNU (addNU (u:us) m) = evalNU (u:us) + evalNU m?

    -- LADO IZQUIERDO

        evalNU (addNU (u:us) m)
    =                               (addNU.2)
        evalNU (u : addNU us m)
    =                               (evalNU.2)
        1 + evalNU (addNU us m)
    =                               (HI)
        1 + evalNU us + evalNU m

    -- LADO DERECHO

        evalNU (u:us) + evalNU m
    =                               (evalNU.2)
        1 + evalNU us + evalNU m

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.B.III

nu2n . n2nu = id 

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿Para todo n1. nu2n . n2nu n1 = id n1?

    Sea n un elemento cualquiera de tipo N. Por principio de inducción en la estructura n
    es equivalente demostrar que:

    Caso base (n = Z):
        ¿nu2n . n2nu Z = id Z?

    Caso inductivo (n = (S n')):
        Hipotesis inductiva:
            ¡nu2n . n2nu n' = id n'!

        Tesis inductiva:
            ¿nu2n . n2nu (S n') = id (S n')?

    Demostración caso base:
        ¿nu2n . n2nu Z = id Z?

    -- LADO IZQUIERDO

        nu2n . n2nu Z
    =                       (.)
        nu2n (n2nu Z)
    =                       (n2nu.1)
        nu2n []
    =                       (nu2n.1)
        Z

    -- LADO DERECHO

        id Z
    =                       (id.1)
        Z

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿nu2n . n2nu (S n') = id (S n')?

    -- LADO IZQUIERDO

        nu2n . n2nu (S n')
    =                               (.)
        nu2n (n2nu (S n'))
    =                               (n2nu.2)
        nu2n (() : n2nu n')
    =                               (nu2n.2)
        S (nu2n (n2nu n'))
    =                               (HI)
        S (id n')
    =                               (id.1)
        S n'
    =                               (aritmética)
        (S n')

    -- LADO DERECHO

        id (S n')
    =                               (id.1)
        (S n')

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.B.IV

n2nu . nu2n = id

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿Para todo n1. n2nu . nu2n n1 = id n1?

    Sea n un elemento cualquiera de tipo NU. Por principio de inducción en la estructura n
    es equivalente demostrar que:

    Caso base (n = []):
        ¿n2nu . nu2n [] = id []?

    Caso inductivo (n = (u:us)):
        Hipotesis inductiva:
            ¡n2nu . nu2n us = id us!

        Tesis inductiva:
            ¿n2nu . nu2n (u:us) = id (u:us)?

    Demostración caso base:
        ¿n2nu . nu2n [] = id []?

    -- LADO IZQUIERDO

        n2nu . nu2n []
    =                           (.)
        n2nu (nu2n [])
    =                           (nu2n.1)
        n2nu Z
    =                           (n2nu.1)
        []

    -- LADO DERECHO

        id []
    =                           (id.1)
        []

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿n2nu . nu2n (u:us) = id (u:us)?

    -- LADO IZQUIERDO

        n2nu . nu2n (u:us)
    =                           (.)
        n2nu (nu2n (u:us))
    =                           (nu2n.2)
        n2nu (S (nu2n us))
    =                           (n2nu.2)
        () : n2nu (nu2n us)
    =                           (.)
        () : n2nu . nu2n us
    =                           (HI)
        () : id us
    =                           (id.1)
        () : us
    =                           (aritmética)
        (():us)                 -- Es lo mismo que escribir (u:us), porque está garantizado por la definición del tipo NU.

    -- LADO DERECHO

        id (u:us)
    =                           (id.1)
        (u:us)
    =                           (NU)
        (():us)

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


> Ejercicio 3:

type NBin = [DigBin]
data DigBin = O | I

-- 3.A.I:

evalNB :: NBin -> Int
evalNB []       = 0
evalNB (nb:nbs) = dbAsInt nb + (2 * evalNB nbs)

dbAsInt :: DigBin -> Int
dbAsInt I = 1
dbAsInt O = 0


-- 3.A.II:

normalizarNB :: NBin -> NBin
normalizarNB []       = []
normalizarNB (nb:nbs) = normalizarDB nb (normalizarNB nbs)

normalizarDB :: DigBin -> NBin -> NBin
normalizarDB O  [] = []
normalizarDB db nbs = db : nbs


-- 3.A.III:

succNB :: NBin -> NBin
succNB []      = [I]
succNB (O:nbs) = I : nbs
succNB (I:nbs) = O : succNB nbs


-- 3.A.IV:

addNB :: NBin -> NBin -> NBin
addNB nb1 nb2 = addNBConCarry nb1 nb2 O

addNBConCarry :: NBin -> NBin -> DigBin -> NBin
addNBConCarry []       []       O = []
addNBConCarry []       []       I = [I]
addNBConCarry []       (mb:mbs) c = fst (addDBConCarry O  mb c) : addNBConCarry []  mbs (snd (addDBConCarry O  mb c))
addNBConCarry (nb:nbs) []       c = fst (addDBConCarry nb O  c) : addNBConCarry nbs []  (snd (addDBConCarry nb O  c))
addNBConCarry (nb:nbs) (mb:mbs) c = fst (addDBConCarry nb mb c) : addNBConCarry nbs mbs (snd (addDBConCarry nb mb c))

addDBConCarry :: DigBin -> DigBin -> DigBin -> (DigBin, DigBin)
addDBConCarry I I O = (O, I)
addDBConCarry I I I = (I, I)
addDBConCarry O O c = (c, O)
addDBConCarry _ _ I = (O, I)
addDBConCarry _ _ O = (I, O)


-- 3.A.V:

nb2n :: NBin -> N
nb2n []      = Z
nb2n (O:nbs) = dobleN (nb2n nbs)
nb2n (I:nbs) = S (dobleN (nb2n nbs))

dobleN :: N -> N
dobleN Z     = Z
dobleN (S n) = S (S (dobleN n))


-- 3.A.VI:

n2nb :: N -> NBin
n2nb Z     = []
n2nb (S n) = succNB (n2nb n)


-- 3.B.I:

evalNB . normalizarNB = evalNB

Demostración:
    Por principio de extensionalidad es equivalente demostrar que:
    ¿Para todo n. ¿evalNB . normalizarNB n = evalNB n?

    Sea nbs un elemento cualquiera de tipo NBin (el cual está normalizado). Por principio de inducción 
    en la estructura nbs es equivalente demostrar que:

    Caso base (nbs = []):
        ¿evalNB . normalizarNB [] = evalNB []?

    Caso inductivo (nbs = (nb:nbs')):
        Hipotesis inductiva:
            ¡evalNB . normalizarNB nbs' = evalNB nbs'!

        Tesis inductiva:
            ¿evalNB . normalizarNB (nb:nbs') = evalNB (nb:nbs')?

    Demostración caso base:
        ¿evalNB . normalizarNB [] = evalNB []?

    -- LADO IZQUIERDO

        evalNB . normalizarNB []
    =                                   (.)
        evalNB (normalizarNB [])
    =                                   (normalizarNB.1)
        evalNB []

    -- LADO DERECHO

        evalNB []

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿evalNB . normalizarNB (nb:nbs') = evalNB (nb:nbs')?

    -- LADO IZQUIERDO

        evalNB . normalizarNB (nb:nbs')
    =                                                   (.)
        evalNB (normalizarNB (nb:nbs'))
    =                                                   (normalizarNB.2)
        evalNB (normalizarDB nb (normalizarNB nbs'))
    =                                                   (Lema EvalNorm)
        dbAsInt nb + (2 * evalNB (normalizarNB nbs'))
    =                                                   (.)
        dbAsInt nb + (2 * evalNB . normalizarNB nbs')
    =                                                   (HI)
        dbAsInt nb + (2 * evalNB nbs')

    -- LADO DERECHO

        evalNB (nb:nbs')
    =                                                   (evalNB.2)
        dbAsInt nb + (2 * evalNB nbs')

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema EvalNorm: ¿evalNB (normalizarDB xb nys) = dbAsInt xb + (2 * evalNB nys)?

    Demostración:
        Sea db un elemento cualquiera de tipo DigBin, y nbs un elemento cualquiera de tipo NBin (el cual está normalizado).
        Por principio de inducción en la estructura nbs es equivalente demostrar que:

        Caso base (nbs = []):
            ¿evalNB (normalizarDB db []) = dbAsInt db + (2 * evalNB [])?

        Caso inductivo (nbs = (nb:nbs'))
            Hipotesis inductiva:
                ¡evalNB (normalizarDB db nbs') = dbAsInt db + (2 * evalNB nbs')!

            Tesis inductiva:
                ¿evalNB (normalizarDB db (nb:nbs')) = dbAsInt db + (2 * evalNB (nb:nbs'))?

        Demostración caso base:
            ¿evalNB (normalizarDB db []) = dbAsInt db + (2 * evalNB [])?

            Caso db = O: ¿evalNB (normalizarDB O []) = dbAsInt O + (2 * evalNB [])?

            -- LADO IZQUIERDO

                evalNB (normalizarDB O [])
            =                                   (normalizarDB.1)
                evalNB []
            =                                   (evalNB.1)
                0

            -- LADO DERECHO

                dbAsInt O + (2 * evalNB [])
            =                                   (evalNB.1)
                dbAsInt O + (2 * 0)
            =                                   (aritmética)
                dbAsInt O + 0
            =                                   (aritmética)
                dbAsInt O
            =                                   (dbAsInt.2)
                0

                -- Ambos lados llegan a lo mismo, el caso es válido.

            Caso db = I: ¿evalNB (normalizarDB I []) = dbAsInt I + (2 * evalNB [])?

            -- LADO IZQUIERDO

                evalNB (normalizarDB I [])
            =                                   (normalizarDB.2)
                evalNB (I:[])
            =                                   (evalNB.2)
                dbAsInt I + (2 * evalNB [])

            -- LADO DERECHO

                dbAsInt I + (2 * evalNB [])

                -- Ambos lados llegan a lo mismo, el caso es válido.

        Demostración caso inductivo:
            ¿evalNB (normalizarDB db (nb:nbs')) = dbAsInt db + (2 * evalNB (nb:nbs'))?

        -- LADO IZQUIERDO

            evalNB (normalizarDB db (nb:nbs'))
        =                                               (normalizarDB.2) -- Aca ya sé que la lista no es [], por eso cae en el segundo caso.
            evalNB (db:(nb:nbs'))
        =                                               (evalNB.2)
            dbAsInt db + (2 * evalNB (nb:nbs'))

        -- LADO DERECHO

            dbAsInt db + (2 * evalNB (nb:nbs'))

            -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 3.B.II:

evalNB . succNB = (+1) . evalNB 

Demostración:
    Por principio de extensionalidad es equivalente demostrar que
    ¿Para todo n. evalNB . succNB n = (+1) . evalNB n?

    Sea nbs un elemento cualquiera de tipo NBin (el cual está normalizado). Por principio de inducción
    en la estructura nbs es equivalente demostrar que:

    Caso base (nbs = []):
        ¿evalNB . succNB [] = (+1) . evalNB []?

    Caso inductivo (nbs = (nb:nbs')):
        Hipotesis inductiva:
            ¡evalNB . succNB nbs' = (+1) . evalNB nbs'!

        Tesis inductiva:
            ¿evalNB . succNB (nb:nbs') = (+1) . evalNB (nb:nbs')?

    Demostración caso base:
        ¿evalNB . succNB [] = (+1) . evalNB []?

    -- LADO IZQUIERDO

        evalNB . succNB []
    =                               (.)
        evalNB (succNB [])
    =                               (succNB.1)
        evalNB [I]
    =                               (:)
        evalNB (I:[])
    =                               (evalNB.2)
        dbAsInt I + (2 * evalNB [])
    =                               (dbAsInt.1)
        1 + (2 * evalNB [])
    =                               (evalNB.1)
        1 + (2 * 0)
    =                               (aritmética)
        1 + 0
    =                               (aritmética)
        1

    -- LADO DERECHO

        (+1) . evalNB []
    =                               (.)
        (+1) (evalNB [])
    =                               (evalNB.1)
        (+1) (0)
    =                               (+)
        1 + 0
    =                               (aritmética)
        1

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿evalNB . succNB (nb:nbs') = (+1) . evalNB (nb:nbs')?

        Sea nb' un elemento cualquiera de tipo DigBit, se verá que: evalNB . succNB (nb':nbs') = (+1) . evalNB (nb':nbs')

        Caso nb' = O:

        -- LADO IZQUIERDO

            evalNB . succNB (O:nbs')
        =                                           (.)
            evalNB (succNB (O:nbs'))
        =                                           (succNB.2)
            evalNB (I:nbs')
        =                                           (evalNB.2)
            dbAsInt I + (2 * evalNB nbs')
        =                                           (dbAsInt.1)
            1 + (2 * evalNB nbs')

        -- LADO DERECHO

            (+1) . evalNB (O:nbs')
        =                                           (.)
            (+1) (evalNB (O:nbs'))
        =                                           (+)
            1 + (evalNB (O:nbs'))
        =                                           (evalNB.2)
            1 + (dbAsInt O + (2 * evalNB nbs'))
        =                                           (dbAsInt.0)
            1 + (0 + (2 * evalNB nbs'))
        =                                           (aritmética)
            1 + (2 * evalNB nbs')

            -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso nb' = I:

        -- LADO IZQUIERDO

            evalNB . succNB (I:nbs')
        =                                           (.)
            evalNB (succNB (I:nbs'))
        =                                           (succNB.3)
            evalNB (O : succNB nbs')
        =                                           (evalNB.2)
            dbAsInt O + (2 * evalNB (succNB nbs'))
        =                                           (.)
            dbAsInt O + (2 * evalNB . succNB nbs')
        =                                           (HI)
            dbAsInt O + (2 * ((+1) . evalNB nbs'))
        =                                           (.)
            dbAsInt O + (2 * ((+1) (evalNB nbs')))
        =                                           (+)
            dbAsInt O + (2 * (1 + (evalNB nbs')))
        =                                           (dbAsInt.2)
            O + (2 * (1 + (evalNB nbs')))
        =                                           (aritmética)
            2 * (1 + (evalNB nbs'))
        =                                           (aritmética)
            (2 * 1) + (2 * (evalNB nbs'))
        =                                           (aritmética)
            2 + (2 * (evalNB nbs'))
        =                                           (aritmética)
            2 + (2 * evalNB nbs')

        -- LADO DERECHO

            (+1) . evalNB (I:nbs')
        =                                           (.)
            (+1) (evalNB (I:nbs'))
        =                                           (+)
            1 + (evalNB (I:nbs'))
        =                                           (evalNB.2)
            1 + (dbAsInt I + (2 * evalNB nbs'))
        =                                           (dbAsInt.1)
            1 + (1 + (2 * evalNB nbs'))
        =                                           (aritmética)
            1 + 1 + (2 * evalNB nbs')
        =                                           (aritmética)
            2 + (2 * evalNB nbs')

            -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 3.B.III:

¿Para todo n1. para todo n2. evalNB (addNB n1 n2) = evalNB n1 + evalNB n2?

Demostración:
    Sea nbs, mbs dos elementos cualquiera de tipo NBin (los cuales estan normalizados). Por principio de
    inducción en la estructura nbs es equivalente demostrar que:

    Caso base (nbs = []):
        ¿evalNB (addNB [] mbs) = evalNB [] + evalNB mbs?

    Caso inductivo (nbs = (nb:nbs')):
        Hipotesis inductiva:
            ¡evalNB (addNB nbs' mbs) = evalNB nbs' + evalNB mbs!

        Tesis inductiva:
            ¿evalNB (addNB (nb:nbs') mbs) = evalNB (nb:nbs') + evalNB mbs?

    Demostración caso base:
        ¿evalNB (addNB [] mbs) = evalNB [] + evalNB mbs?

    -- LADO IZQUIERDO
        
        evalNB (addNB [] mbs)
    =                                                   (addNB.1)
        evalNB (addNBConCarry [] mbs O)
    =                                                   (Lema AddNil)
        evalNB mbs

    -- LADO DERECHO

        evalNB [] + evalNB mbs
    =                                   (evalNB.1)
        0 + evalNB mbs
    =                                   (aritmética)
        evalNB mbs

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿evalNB (addNB (nb:nbs') mbs) = evalNB (nb:nbs') + evalNB mbs?

    Caso mbs = []:

    -- LADO IZQUIERDO

        evalNB (addNB (nb:nbs') [])
    =                                                                                               (addNB.1)
        evalNB (addNBConCarry (nb:nbs') [] O)
    =                                                                                               (addNBConCarry.4)
        evalNB (fst (addDBConCarry nb O O)  : addNBConCarry nbs' [] (snd (addDBConCarry nb O O)))
    =                                                                                               (Lema AddOO)
        evalNB (fst (nb, O) : addNBConCarry nbs' [] (snd (nb, O)))
    =                                                                                               (fst.1)
        evalNB (nb : addNBConCarry nbs' [] (snd (nb, O)))
    =                                                                                               (snd.1)
        evalNB (nb : addNBConCarry nbs' [] O)
    =                                                                                               (evalNB.2)
        dbAsInt nb + 2 * evalNB (addNBConCarry nbs' [] O)
    =                                                                                               (addNB, pero inversa)
        dbAsInt nb + 2 * evalNB (addNB nbs' [])
    =                                                                                               (HI, siendo mbs = [])
        dbAsInt nb + 2 * (evalNB nbs' + evalNB [])
    =                                                                                               (evalNB.1)
        dbAsInt nb + 2 * (evalNB nbs' + 0)
    =                                                                                               (aritmética)
        dbAsInt nb + 2 * (evalNB nbs')

        -- Ambos lados llegan a lo mismo, el caso es válido.

    -- LADO DERECHO

        evalNB (nb:nbs') + evalNB []
    =                                                   (evalNB.1)
        evalNB (nb:nbs') + 0 
    =                                                   (aritmética)
        evalNB (nb:nbs') 
    =                                                   (evalNB.2)
        dbAsInt nb + 2 * (evalNB nbs')

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Caso mbs = (mb:mbs'):

    -- LADO IZQUIERDO:

        evalNB (addNB (nb:nbs') (mb:mbs'))
    =                                                                       (addNB.1)
        evalNB (addNBConCarry (nb:nbs') (mb:mbs') O)
    =                                                                       (Lema AddCarryO)
        dbAsInt nb + dbAsInt mb + 2 * evalNB (addNBConCarry nbs' mbs' O)
    =                                                                       (addNB, pero inversa)
        dbAsInt nb + dbAsInt mb + 2 * evalNB (addNB nbs' mbs')
    =                                                                       (HI)
        dbAsInt nb + dbAsInt mb + 2 * (evalNB nbs' + evalNB mbs')
    =                                                                       (aritmética)
        dbAsInt nb + dbAsInt mb + 2 * (evalNB nbs') + 2 * (evalNB mbs')

    -- LADO DERECHO:

        evalNB (nb:nbs') + evalNB (mb:mbs')
    =                                                                       (evalNB.2)
        dbAsInt nb + 2 * (evalNB nbs') + evalNB (mb:mbs')
    =                                                                       (evalNB.2)
        dbAsInt nb + 2 * (evalNB nbs') + dbAsInt mb + 2 * (evalNB mbs')
    =                                                                       (aritmética)
        dbAsInt nb + dbAsInt mb + 2 * (evalNB nbs') + 2 * (evalNB mbs')

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema AddOO: ¿addDBConCarry nb O O = (nb , O)?

    Demostración:
        Sea nb un elemento cualquiera de tipo DigBin.

        Caso nb = I:

        -- LADO IZQUIERDO

            addDBConCarry I O O
        =                               (addNBConCarry.5)
            (I, O)

        -- LADO DERECHO

            (I , O)

            -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso nb = O:

        -- LADO IZQUIERDO

            addDBConCarry O O O
        =                               (addNBConCarry.5)
            (O, O)

        -- LADO DERECHO

            (O , O)

            -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema AddCarryO: ¿evalNB (addNBConCarry (nb:nbs) (mb:mbs) O) = dbAsInt nb + dbAsInt mb + 2 * evalNB (addNBConCarry nbs mbs O)?

        Demostración: TERMINAR 


-- 3.B.IV:

nb2n . n2nb = id

Demostración:
    Por principio de extensionalidad es equivalente demostrar que
    ¿Para todo n1. nb2n . n2nb n1 = id n1?

    Sea n un elemento cualquiera de tipo N. Por principio de inducción en la 
    estructura n es equivalente demostrar que:

    Caso base (n = Z):
        ¿nb2n . n2nb Z = id Z?

    Caso inductivo (n = (S n')):
        Hipotesis inductiva:
            ¡nb2n . n2nb n' = id n'!

        Tesis inductiva:
            ¿nb2n . n2nb (S n') = id (S n')?

    Demostración caso base:
        ¿nb2n . n2nb Z = id Z?

    -- LADO IZQUIERDO

        nb2n . n2nb Z
    =                           (.)
        nb2n (n2nb Z)
    =                           (n2nb.1)
        nb2n []
    =                           (nb2n.1)
        Z

    -- LADO DERECHO

        id Z
    =                           (id.1)
        Z

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿nb2n . n2nb (S n') = id (S n')?

    -- LADO IZQUIERDO

        nb2n . n2nb (S n')
    =                               (.)
        nb2n (n2nb (S n'))
    =                               (n2nb.2)
        nb2n (succNB (n2nb n'))
    =                               (Lema SuccNB2N)
        S (nb2n (n2nb n'))
    =                               (.)
        S (nb2n . n2nb n')
    =                               (HI)
        S (id n')
    =                               (id.1)
        S (n')
    =                               (aritmética)
        (S n')

    -- LADO DERECHO

        id (S n')
    =                               (id.1)
        (S n')

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema SuccNB2N: ¿nb2n (succNB n) = S (nb2n n)?

    Demostración:
        Sea nbs un elemento cualquiera de tipo NBin. Por principio de inducción en la
        estructura nbs es equivalente demostrar que:

        Caso base (nbs = []):
            ¿nb2n (succNB []) = S (nb2n [])?

        Caso inductivo (nbs = (nb:nbs')):
            Hipotesis inductiva:
                ¡nb2n (succNB nbs') = S (nb2n nbs')!

            Tesis inductiva:
                ¿nb2n (succNB (nb:nbs')) = S (nb2n (nb:nbs'))?

        Demostración caso base:
            ¿nb2n (succNB []) = S (nb2n [])?

        -- LADO IZQUIERDO

            nb2n (succNB [])
        =                           (succNB.1)
            nb2n [I]
        =                           (:)
            nb2n (I:[])
        =                           (nb2n.3)
            S (dobleN (nb2n []))
        =                           (nb2n.1)
            S (dobleN Z)
        =                           (dobleN.1)
            S Z

        -- LADO DERECHO

            S (nb2n [])
        =                           (nb2n.1)
            S Z

            -- Ambos lados llegan a lo mismo, el caso es válido.

        Demostración caso inductivo:
            ¿nb2n (succNB (nb:nbs')) = S (nb2n (nb:nbs'))?

            Sea nb' un elemento cualquiera de tipo DigBit, se verá que: 
            nb2n (succNB (nb':nbs')) = S (nb2n (nb':nbs'))

            Caso nb' = O:

            -- LADO IZQUIERDO

                nb2n (succNB (O:nbs'))
            =                                   (succNB.2)
                nb2n (succDB O nbs')
            =                                   (succDB.1)
                nb2n (I:nbs')
            =                                   (nb2n.3)
                S (dobleN (nb2n nbs'))

            -- LADO DERECHO

                S (nb2n (O:nbs'))
            =                                   (nb2n.2)
                S (dobleN (nb2n nbs'))

                -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

            Caso nb' = I:

            -- LADO IZQUIERDO

                nb2n (succNB (I:nbs'))
            =                                   (succNB.2)
                nb2n (succDB I nbs')
            =                                   (succDB.2)
                nb2n (O:(succNB nbs'))
            =                                   (nb2n.2)
                dobleN (nb2n (succNB nbs'))
            =                                   (HI)
                dobleN (S (nb2n nbs'))
            =                                   (dobleN.2)
                S (S (dobleN (nb2n nbs')))

            -- LADO DERECHO

                S (nb2n (I:nbs'))
            =                                   (nb2n.3)
                S (S (dobleN (nb2n nbs')))

                -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 3.B.V:

normalizarNB . normalizarNB = normalizarNB

Demostración:
    Por principio de extensionalidad es equivalente demostrar que
    ¿Para todo n. normalizarNB . normalizarNB = normalizarNB?

    Sea nbs un elemento cualquiera de tipo Nbin (el cual está normalizado). Por principio de inducción
    en la estructura nbs es equivalente demostrar que:

    Caso base (nbs = []):
        ¿normalizarNB . normalizarNB [] = normalizarNB []?

    Caso inductivo (nbs = (nb:nbs')):
        Hipotesis inductiva:
            ¡normalizarNB . normalizarNB nbs' = normalizarNB nbs'!

        Tesis inductiva:
            ¿normalizarNB . normalizarNB (nb:nbs') = normalizarNB (nb:nbs')?

    Demostración caso base:
        ¿normalizarNB . normalizarNB [] = normalizarNB []?

    -- LADO IZQUIERDO

        normalizarNB . normalizarNB []
    =                                       (.)
        normalizarNB (normalizarNB [])
    =                                       (normalizarNB.1)
        normalizarNB []
    =                                       (normalizarNB.1)
        []

    -- LADO DERECHO

        normalizarNB []
    =                                       (normalizarNB.1)
        []

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿normalizarNB . normalizarNB (nb:nbs') = normalizarNB (nb:nbs')?

    -- LADO IZQUIERDO

        normalizarNB . normalizarNB (nb:nbs')
    =                                                       (.)
        normalizarNB (normalizarNB (nb:nbs'))
    =                                                       (normalizarNB.2)
        normalizarNB (normalizarDB nb (normalizarNB nbs'))
    =                                                       (Lema NormDB)
        normalizarDB nb (normalizarNB (normalizarNB nbs'))
    =                                                       (.)
        normalizarDB nb (normalizarNB . normalizarNB nbs')
    =                                                       (HI)
        normalizarDB nb (normalizarNB nbs')

    -- LADO DERECHO

        normalizarNB (nb:nbs')
    =                                                       (normalizarNB.2)
        normalizarDB nb (normalizarNB nbs')

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema NormDB: ¿normalizarNB (normalizarDB d ns) = normalizarDB d (normalizarNB ns)?

    Demostración:
        Sea db un elemento cualquiera de tipo DigBit, nbs un elemento cualquiera de tipo NBin (el cual está normalizado).
        Por principio de inducción en la estructura nbs es equivalente demostrar que:

        Caso base (nbs = []):
            ¿normalizarNB (normalizarDB db []) = normalizarDB db (normalizarNB [])?

        Caso inductivo (nbs = (nb:nbs')):
            Hipotesis inductiva:
                ¡normalizarNB (normalizarDB db nbs') = normalizarDB db (normalizarNB nbs')!

            Tesis inductiva:
                ¿normalizarNB (normalizarDB db (nb:nbs')) = normalizarDB db (normalizarNB (nb:nbs'))?

        Demostración caso base:
            ¿normalizarNB (normalizarDB db []) = normalizarDB db (normalizarNB [])?

            Caso db = O:

            -- LADO IZQUIERDO

                normalizarNB (normalizarDB O [])
            =                                           (normalizarDB.1)
                normalizarNB []
            =                                           (normalizarNB.1)
                []

            -- LADO DERECHO

                normalizarDB O (normalizarNB [])
            =                                           (normalizarNB.1)
                normalizarDB O []
            =                                           (normalizarDB.1)
                []

                -- Ambos lados llegan a lo mismo, el caso es válido.

            Caso db = I:

            -- LADO IZQUIERDO

                normalizarNB (normalizarDB I [])
            =                                           (normalizarDB.2)
                normalizarNB (I:[])
            =                                           (normalizarNB.2)
                normalizarDB I (normalizarNB [])

            -- LADO DERECHO

                normalizarDB I (normalizarNB [])

                -- Ambos lados llegan a lo mismo, el caso es válido.

        Demostración caso inductivo:
            ¿normalizarNB (normalizarDB db (nb:nbs')) = normalizarDB db (normalizarNB (nb:nbs'))?

        -- LADO IZQUIERDO

            normalizarNB (normalizarDB db (nb:nbs'))
        =                                                           (normalizarDB.2)
            normalizarNB (db:(nb:nbs'))
        =                                                           (normalizarNB.2)
            normalizarDB db (normalizarNB (nb:nbs'))

        -- LADO DERECHO

            normalizarDB db (normalizarNB (nb:nbs'))

            -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 3.C.I:

n2nb . nb2n = id

Demostración:



-- 3.C.II:

n2nb . nb2n = normalizarNB

Demostración:



> Ejercicio 4:

type NDec = [DigDec]
data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9

-- 4.A.I:

evalND :: NDec -> Int
evalND []       = 0
evalND (nd:nds) = ddAsInt nd + (10 * evalND nds)

ddAsInt :: DigDec -> Int
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


-- 4.A.II:

normalizarND :: NDec -> NDec
normalizarND []       =
normalizarND (nd:nds) =


-- 4.A.III:

succNDec :: NDec -> NDec
succNDec []       =
succNDec (nd:nds) =


-- 4.A.IV:

addNDec :: NDec -> NDec -> NDec
addNDec []       md =
addNDec (nd:nds) md =


-- 4.A.V:

nd2nb :: NDec -> NBin
nd2nb []       =
nd2nb (nd:nds) =


-- 4.A.VI:

nb2nd :: NBin -> NDec
nb2nd []       =
nb2nd (nb:nbs) =


-- 4.B.I:

evalNDec . succNDec = (+1) . evalNDec


-- 4.B.II:

¿Para todo n1. para todo n2. evalNDec (addNDec n1 n2) = evalNDec n1 + evalNDec n2?


-- 4.B.III:

nd2nb . nb2nd = normalizarNB


-- 4.B.IV:

nb2nd . nd2nb = id


> Ejercicio 5:

Representación en N:
    - 17: (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))))))))
    - 42: (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
          (S (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))))))))))))))))))))))))))))))))

Representación en NU:
    - 17: [(), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()]
    - 42: [(), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (),
           (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (),
           (), (), (), (), (), (), (), ()]

Representación en NBin:
    - 17: [I, O, O, O, I]
    - 42: [O, I, O, I, O, I]

Representación en NDec:
    - 17: [D7, D1]
    - 42: [D2, D4]


# SECCIÓN 3

> Ejercicio 1:

data ExpA = Cte Int     
          | Suma ExpA ExpA
          | Prod ExpA ExpA 

-- 1.A.I

evalExpA :: ExpA -> Int
evalExpA (Cte  n)     = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2


-- 1.A.II

simplificarExpA :: ExpA -> ExpA
simplificarExpA (Cte  n)     = Cte n
simplificarExpA (Suma e1 e2) = simplificarSuma (simplificarExpA e1) (simplificarExpA e2)
simplificarExpA (Prod e1 e2) = simplificarProd (simplificarExpA e1) (simplificarExpA e2)

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma e1       (Cte 0) = e1
simplificarSuma (Cte  0) e2      = e2
simplificarSuma e1       e2      = Suma e1 e2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd _        (Cte 0) = Cte 0
simplificarProd (Cte  0) _       = Cte 0
simplificarProd e1       (Cte 1) = e1
simplificarProd (Cte  1) e2      = e2
simplificarProd e1       e2      = Prod e1 e2


-- 1.A.III

cantidadDeSumaCero :: ExpA -> Int
cantidadDeSumaCero (Cte  n)     = 0
cantidadDeSumaCero (Suma e1 e2) = unoSiSumaCero e1 e2 + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
cantidadDeSumaCero (Prod e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2

unoSiSumaCero :: ExpA -> ExpA -> Int
unoSiSumaCero (Cte 0) _      = 1
unoSiSumaCero _      (Cte 0) = 1
unoSiSumaCero _       _      = 0


-- 1.B.I

evalExpA . simplificarExpA = evalExpA

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿para todo e1. (evalExpA . simplificarExpA) e1 = evalExpA e1? 

    Sea e un elemento cualquiera de tipo ExpA. Por principio de inducción sobre la 
    estructura e es equivalente demostrar:

    Caso base (e = Cte n):
        ¿(evalExpA . simplificarExpA) (Cte n) = evalExpA (Cte n)?


    Caso inductivo 1 (e = Suma e' e''):
        Hipotesis inductiva 1.1:
            ¡(evalExpA . simplificarExpA) e' = evalExpA e'!

        Hipotesis inductiva 1.2:
            ¡(evalExpA . simplificarExpA) e'' = evalExpA e''!

        Tesis inductiva 1:
            ¿(evalExpA . simplificarExpA) (Suma e' e'') = evalExpA (Suma e' e'')?


    Caso inductivo 2 (e = Prod e' e''):
        Hipotesis inductiva 1.2:
            ¡(evalExpA . simplificarExpA) e' = evalExpA e'!

        Hipotesis inductiva 2.2:
            ¡(evalExpA . simplificarExpA) e'' = evalExpA e''!

        Tesis inductiva 2:
            ¿(evalExpA . simplificarExpA) (Prod e' e'') = evalExpA (Prod e' e'')?

    
    Demostración caso base:
        ¿(evalExpA . simplificarExpA) (Cte n) = evalExpA (Cte n)?

    -- LADO IZQUIERDO

        (evalExpA . simplificarExpA) (Cte n)
    =                                               (.)
        evalExpA (simplificarExpA (Cte n))
    =                                               (simplificarExpA.1)
        evalExpA (Cte n)

    -- LADO DERECHO

        evalExpA (Cte n)

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 1:
        ¿(evalExpA . simplificarExpA) (Suma e' e'') = evalExpA (Suma e' e'')?

    -- LADO IZQUIERDO

        (evalExpA . simplificarExpA) (Suma e' e'')
    =                                                                           (.)
        evalExpA (simplificarExpA (Suma e' e''))
    =                                                                           (simplificarExpA.2)
        evalExpA (simplificarSuma (simplificarExpA e') (simplificarExpA e''))
    =                                                                           (Lema EvalSimpSuma)
        evalExpA (simplificarExpA e') + evalExpA (simplificarExpA e'')
    =                                                                           (.)
        (evalExpA . simplificarExpA) e' + evalExpA (simplificarExpA e'')
    =                                                                           (HI 1.1)
        evalExpA e' + evalExpA (simplificarExpA e'')
    =                                                                           (.)
        evalExpA e' + (evalExpA . simplificarExpA) e''
    =                                                                           (HI 1.2)
        evalExpA e' + evalExpA e''

    -- LADO DERECHO

        evalExpA (Suma e' e'')
    =                                                                           (evalExpA.2)
        (evalExpA e') + (evalExpA e'')
    =                                                                           (aritmética)
        evalExpA e' + evalExpA e''

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 2:
        ¿(evalExpA . simplificarExpA) (Prod e' e'') = evalExpA (Prod e' e'')?

    -- LADO IZQUIERDO

        (evalExpA . simplificarExpA) (Prod e' e'')
    =                                                                           (.)
        evalExpA (simplificarExpA (Prod e' e''))
    =                                                                           (simplificarExpA.3)
        evalExpA (simplificarProd (simplificarExpA e') (simplificarExpA e''))
    =                                                                           (Lema EvalSimpProd)
        evalExpA (simplificarExpA e') * evalExpA (simplificarExpA e'')
    =                                                                           (.)
        (evalExpA . simplificarExpA) e' * evalExpA (simplificarExpA e'')
    =                                                                           (HI 2.1)
        evalExpA e' * evalExpA (simplificarExpA e'')
    =                                                                           (.)
        evalExpA e' * (evalExpA . simplificarExpA) e''
    =                                                                           (HI 2.2)
        evalExpA e' * evalExpA e''

    -- LADO DERECHO

        evalExpA (Prod e' e'')
    =                                                                           (evalExpA.3)
        (evalExpA e') * (evalExpA e'')
    =                                                                           (aritmética)
        evalExpA e' * evalExpA e''

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema EvalSimpSuma: para todo e1. para todo e2. evalExpA (simplificarSuma e1 e2) = evalExpA e1 + evalExpA e2

    Demostración:
        Sea e' y e'' elementos cualquiera de tipo ExpA. Se verá que:
        evalExpA (simplificarSuma e' e'') = evalExpA e' + evalExpA e''

        Caso 1 (e'' = (Cte 0)):

        -- LADO IZQUIERDO

            evalExpA (simplificarSuma e' (Cte 0))
        =                                           (simplificarSuma.1)
            evalExpA e'

        -- LADO DERECHO

            evalExpA e' + evalExpA (Cte 0)
        =                                           (evalExpA.1)
            evalExpA e' + 0
        =                                           (aritmética)
            evalExpA e'

            -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 2 (e' = (Cte 0)):

        -- LADO IZQUIERDO

            evalExpA (simplificarSuma (Cte 0) e'')
        =                                           (simplificarSuma.2)
            evalExpA e''

        -- LADO DERECHO

            evalExpA (Cte 0) + evalExpA e''
        =                                           (evalExpA.1)
            0 + evalExpA e''
        =                                           (aritmética)
            evalExpA e''

            -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 3 (e' /= (Cte 0), e'' /= (Cte 0)):

        -- LADO IZQUIERDO

            evalExpA (simplificarSuma e' e'')
        =                                           (simplificarSuma.3)
            evalExpA (Suma e' e'')
        =                                           (evalExpA.2)
            evalExpA e' + evalExpA e''

        -- LADO DERECHO

            evalExpA e' + evalExpA e''

            -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema EvalSimpProd: para todo e1. para todo e2. evalExpA (simplificarProd e1 e2) = evalExpA e1 * evalExpA e2

    Demostración:
        Sea e' y e'' elementos cualquiera de tipo ExpA. Se verá que:
        evalExpA (simplificarProd e' e'') = evalExpA e' * evalExpA e''

        Caso 1 (e'' = (Cte 0)):

        -- LADO IZQUIERDO

            evalExpA (simplificarProd e' (Cte 0))
        =                                           (simplificarProd.1)
            evalExpA (Cte 0)
        =                                           (evalExpA.1)
            0

        -- LADO DERECHO

            evalExpA e' * evalExpA (Cte 0)
        =                                           (evalExpA.1)
            evalExpA e' * 0
        =                                           (aritmética)
            0

            -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 2 (e' = (Cte 0)):

        -- LADO IZQUIERDO

            evalExpA (simplificarProd (Cte 0) e'')
        =                                           (simplificarProd.2)
            evalExpA (Cte 0)
        =                                           (evalExpA.1)
            0

        -- LADO DERECHO

            evalExpA (Cte 0) * evalExpA e''
        =                                           (evalExpA.1)
            0 * evalExpA e''
        =                                           (aritmética)
            0

            -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 3 (e'' = (Cte 1)):

        -- LADO IZQUIERDO

            evalExpA (simplificarProd e' (Cte 1))
        =                                           (simplificarProd.3)
            evalExpA e'

        -- LADO DERECHO

            evalExpA e' * evalExpA (Cte 1)
        =                                           (evalExpA.1)
            evalExpA e' * 1
        =                                           (aritmética)
            evalExpA e'

            -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 4 (e' = (Cte 1)):

        -- LADO IZQUIERDO

            evalExpA (simplificarProd (Cte 1) e'')
        =                                           (simplificarProd.4)
            evalExpA e''

        -- LADO DERECHO

            evalExpA (Cte 1) * evalExpA e''
        =                                           (evalExpA.1)
            1 * evalExpA e''
        =                                           (aritmética)
            evalExpA e''

            -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 5 (e' /= (Cte 0), e' /= (Cte 1), e'' /= (Cte 0), e'' /= (Cte 1)):

        -- LADO IZQUIERDO

            evalExpA (simplificarProd e' e'')
        =                                           (simplificarProd.5)
            evalExpA (Prod e' e'')
        =                                           (evalExpA.3)
            evalExpA e' * evalExpA e''

        -- LADO DERECHO

            evalExpA e' * evalExpA e''

            -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 1.B.II

cantidadDeSumaCero . simplificarExpA = const 0

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que:
    ¿para todo e1. (cantidadDeSumaCero . simplificarExpA) e1 = const 0 e1?

    Sea e un elemento cualquiera de tipo ExpA. Por principio de inducción sobre
    la estructura e es equivalente demostrar que:

    Caso base (e = Cte n):
        ¿(cantidadDeSumaCero . simplificarExpA) (Cte n) = const 0 (Cte n)?


    Caso inductivo 1 (e = Suma e' e''):
        Hipotesis inductiva 1.1:
            ¡(cantidadDeSumaCero . simplificarExpA) e' = const 0 e'!

        Hipotesis inductiva 1.2:
            ¡(cantidadDeSumaCero . simplificarExpA) e'' = const 0 e''!

        Tesis inductiva 1:
            ¿(cantidadDeSumaCero . simplificarExpA) (Suma e' e'') = const 0 (Suma e' e'')?


    Caso inductivo 2 (e = Prod e' e''):
        Hipotesis inductiva 2.1:
            ¡(cantidadDeSumaCero . simplificarExpA) e' = const 0 e'!

        Hipotesis inductiva 2.2:
            ¡(cantidadDeSumaCero . simplificarExpA) e'' = const 0 e''!

        Tesis inductiva 2:
            ¿(cantidadDeSumaCero . simplificarExpA) (Prod e' e'') = const 0 (Prod e' e'')?


    Demostración caso base:
        ¿(cantidadDeSumaCero . simplificarExpA) (Cte n) = const 0 (Cte n)?

    -- LADO IZQUIERDO

        (cantidadDeSumaCero . simplificarExpA) (Cte n)
    =                                                       (.)
        cantidadDeSumaCero (simplificarExpA (Cte n))
    =                                                       (simplificarExpA.1)
        cantidadDeSumaCero (Cte n)
    =                                                       (cantidadDeSumaCero.1)
        0

    -- LADO DERECHO

        const 0 (Cte n)
    =                                                       (const.1)
        0

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 1:
        ¿(cantidadDeSumaCero . simplificarExpA) (Suma e' e'') = const 0 (Suma e' e'')?

    -- LADO IZQUIERDO

        (cantidadDeSumaCero . simplificarExpA) (Suma e' e'')
    =                                                                                           (.)
        cantidadDeSumaCero (simplificarExpA (Suma e' e''))
    =                                                                                           (simplificarExpA.2)
        cantidadDeSumaCero (simplificarSuma (simplificarExpA e') (simplificarExpA e''))
    =                                                                                           (Lema SimplSuma)
        cantidadDeSumaCero (simplificarExpA e') + cantidadDeSumaCero (simplificarExpA e'')
    =                                                                                           (.)
        (cantidadDeSumaCero . simplificarExpA) e' + cantidadDeSumaCero (simplificarExpA e'')
    =                                                                                           (.)
        (cantidadDeSumaCero . simplificarExpA) e' + (cantidadDeSumaCero . simplificarExpA) e''
    =                                                                                           (HI 1.1)
        const 0 e' + (cantidadDeSumaCero . simplificarExpA) e''
    =                                                                                           (HI 1.2)
        const 0 e' + const 0 e''
    =                                                                                           (const.1)
        0 + const 0 e''
    =                                                                                           (const.1)
        0 + 0
    =                                                                                           (aritmética)
        0

    -- LADO DERECHO

        const 0 (Suma e' e'')
    =                                   (const.1)
        0

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo 2:
        ¿(cantidadDeSumaCero . simplificarExpA) (Prod e' e'') = const 0 (Prod e' e'')?

    -- LADO IZQUIERDO

        (cantidadDeSumaCero . simplificarExpA) (Prod e' e'')
    =                                                                                           (.)
        cantidadDeSumaCero (simplificarExpA (Prod e' e''))
    =                                                                                           (simplificarExpA.3)
        cantidadDeSumaCero (simplificarProd (simplificarExpA e') (simplificarExpA e''))
    =                                                                                           (Lema SimplProd)
        cantidadDeSumaCero (simplificarExpA e') + cantidadDeSumaCero (simplificarExpA e'')
    =                                                                                           (.)
        (cantidadDeSumaCero . simplificarExpA) e' + cantidadDeSumaCero (simplificarExpA e'')
    =                                                                                           (.)
        (cantidadDeSumaCero . simplificarExpA) e' + (cantidadDeSumaCero . simplificarExpA) e''
    =                                                                                           (HI 2.1)
        const 0 e' + (cantidadDeSumaCero . simplificarExpA) e''
    =                                                                                           (HI 2.2)
        const 0 e' + const 0 e''
    =                                                                                           (const.1)
        0 + const 0 e''
    =                                                                                           (const.1)
        0 + 0
    =                                                                                           (aritmética)
        0
        
    -- LADO DERECHO

        const 0 (Prod e' e'')
    =                                   (const.1)
        0

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema SimplSuma: para todo e1. para todo e2. cantidadDeSumaCero (simplificarSuma e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2

        Demostración:
            Sea e' y e'' elementos cualquiera de tipo ExpA. Se verá que:
            cantidadDeSumaCero (simplificarSuma e' e'') = cantidadDeSumaCero e' + cantidadDeSumaCero e''

        Caso 1 (e'' = (Cte 0)):

            -- LADO IZQUIERDO

                cantidadDeSumaCero (simplificarSuma e' (Cte 0))
            =                                                           (simplificarSuma.1)
                cantidadDeSumaCero e'

            -- LADO DERECHO

                cantidadDeSumaCero e' + cantidadDeSumaCero (Cte 0)        
            =                                                           (cantidadDeSumaCero.1)
                cantidadDeSumaCero e' + 0        
            =                                                           (aritmética)
                cantidadDeSumaCero e'  

                -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 2 (e' = (Cte 0)):

            -- LADO IZQUIERDO

                cantidadDeSumaCero (simplificarSuma (Cte 0) e'')
            =                                                           (simplificarSuma.1)
                cantidadDeSumaCero e''

            -- LADO DERECHO

                cantidadDeSumaCero (Cte 0) + cantidadDeSumaCero e''        
            =                                                           (cantidadDeSumaCero.1)
                0 + cantidadDeSumaCero e''        
            =                                                           (aritmética)
                cantidadDeSumaCero e''

                -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 3 (e' /= (Cte 0), e'' /= (Cte 0)):

            -- LADO IZQUIERDO

                cantidadDeSumaCero (simplificarSuma e' e'')
            =                                                                           (simplificarSuma.1)
                cantidadDeSumaCero (Suma e' e'')
            =                                                                           (cantidadDeSumaCero.2)
                unoSiSumaCero e' e'' + cantidadDeSumaCero e' + cantidadDeSumaCero e''
            =                                                                           (unoSiSumaCero.3)
                0 + cantidadDeSumaCero e' + cantidadDeSumaCero e''
            =                                                                           (aritmética)
                cantidadDeSumaCero e' + cantidadDeSumaCero e''

            -- LADO DERECHO

                cantidadDeSumaCero e' + cantidadDeSumaCero e''        

                -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema SimplProd: para todo e1. para todo e2. cantidadDeSumaCero (simplificarProd e1 e2) = cantidadDeSumaCero e1 * cantidadDeSumaCero e2
        REVISAR!!!

        Demostración:
            Sea e' y e'' elementos cualquiera de tipo ExpA. Se verá que:
            cantidadDeSumaCero (simplificarProd e' e'') = cantidadDeSumaCero e' * cantidadDeSumaCero e''

        Caso 1 (e'' = (Cte 0)):

            -- LADO IZQUIERDO

                cantidadDeSumaCero (simplificarProd e' (Cte 0))
            =                                                           (simplificarProd.1)
                cantidadDeSumaCero (Cte 0)
            =                                                           (cantidadDeSumaCero.1)
                0

            -- LADO DERECHO

                cantidadDeSumaCero e' * cantidadDeSumaCero (Cte 0)
            =                                                           (cantidadDeSumaCero.1)
                cantidadDeSumaCero e' * 0
            =                                                           (aritmética)
                0

                -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 2 (e' = (Cte 0)):

            -- LADO IZQUIERDO

                cantidadDeSumaCero (simplificarProd (Cte 0) e'')
            =                                                           (simplificarProd.2)
                cantidadDeSumaCero (Cte 0)
            =                                                           (cantidadDeSumaCero.1)
                0

            -- LADO DERECHO

                cantidadDeSumaCero (Cte 0) * cantidadDeSumaCero e''
            =                                                           (cantidadDeSumaCero.1)
                0 * cantidadDeSumaCero e''
            =                                                           (aritmética)
                0

                -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 3 (e'' = (Cte 1)):

            -- LADO IZQUIERDO

                cantidadDeSumaCero (simplificarProd e' (Cte 1))
            =                                                           (simplificarProd.3)
                cantidadDeSumaCero e'

            -- LADO DERECHO

                cantidadDeSumaCero e' * cantidadDeSumaCero (Cte 1)
            =                                                           (cantidadDeSumaCero.1)
                cantidadDeSumaCero e' * 0
            =                                                           (aritmética)
                0

                -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 4 (e' = (Cte 1)):

        



        Caso 5 (e' /= (Cte 0), e' /= (Cte 1), e'' /= (Cte 0), e'' /= (Cte 1)):

        



> Ejercicio 2:

data ExpS = CteS N     
          | SumS ExpS ExpS
          | ProdS ExpS Exps

-- 2.A.I

evalES :: ExpS -> Int
evalES (CteS  n)     = evalES n
evalES (SumS  e1 e2) = evalES e1 + evalES e2
evalES (ProdS e1 e2) = evalES e1 * evalES e2


-- 2.A.II

es2ExpA :: ExpS -> ExpA
es2ExpA (CteS  n)     =
es2ExpA (SumS  e1 e2) =
es2ExpA (ProdS e1 e2) =


-- 2.A.III

expA2es :: ExpA -> ExpS
expA2es (Cte  n)     =
expA2es (Suma e1 e2) =
expA2es (Prod e1 e2) =


-- 2.B.I

evalExpA . es2ExpA = evalES


-- 2.B.II

evalES . expA2es = evalExpA


-- 2.B.III

es2ExpA . expA2es = id


-- 2.B.IV

expA2es . es2ExpA = id


-- IMPLEMENTACIONES ALTERNATIVAS A FUNCIONES:

addNB :: NBin -> NBin -> NBin
addNB []       mbs      = mbs
addNB nbs      []       = nbs
addNB (nb:nbs) (mb:mbs) = addDB nb mb (addNB nbs mbs)

addDB :: DigBin -> DigBin -> NBin -> NBin
addDB I I nbs = O : succNB nbs
addDB O O nbs = O : nbs
addDB _ _ nbs = I : nbs

addNBConCarry :: NBin -> NBin -> DigBin -> NBin
addNBConCarry []       []       O = []
addNBConCarry []       []       I = [I]
addNBConCarry []       (mb:mbs) c = let (s', c') = addDBConCarry O mb c
                                        in s' : addNBConCarry [] mbs c'
addNBConCarry (nb:nbs) []       c = let (s', c') = addDBConCarry nb O c
                                        in s' : addNBConCarry nbs [] c'
addNBConCarry (nb:nbs) (mb:mbs) c = let (s', c') = addDBConCarry nb mb c
                                        in s' : addNBConCarry nbs mbs c'