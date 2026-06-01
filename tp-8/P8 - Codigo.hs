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
    =                                                   (unoSi.True)
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

    Sea as una lista cualquiera (finita y bien definida) y b un elemento del mismo tipo de la lista. Por principio de inducción 
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

¿Para todo x. any (elem x) = elem x . concat?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿Para todo x. para todo xs. any (elem x) xs = elem x . concat xs?

    Sea as una lista cualquiera (finita y bien definida) y b un elemento. Por principio de inducción
    sobre la estructura as es equivalente demostrar que:

    Caso base (as = []):
        ¿any (elem b) [] = elem b . concat []?

    Caso inductivo (as = (a:as')):
        Hipotesis inductiva:
            ¡any (elem b) as' = elem b . concat as'!

        Tesis inductiva:
            ¿any (elem b) (a:as') = elem b . concat (a:as')?

    Demostración caso base:
        ¿any (elem b) [] = elem b . concat []?

    -- LADO IZQUIERDO

        any (elem b) []
    =                           (any.1)
        False

    -- LADO DERECHO

        elem b . concat []
    =                           (.)
        elem b (concat [])
    =                           (concat.1)
        elem b []
    =                           (elem.1)
        False

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿any (elem b) (a:as') = elem b . concat (a:as')?

    -- LADO IZQUIERDO

        any (elem b) (a:as')
    =                                           (any.2)
        elem b a || any (elem b) as'
    =                                           (HI)
        elem b a || elem b . concat as'
    =                                           (.)
        elem b a || elem b (concat as')
    =                                           (Lema ElemB)
        elem b (a ++ concat as')

    -- LADO DERECHO

        elem b . concat (a:as')
    =                                           (.)
        elem b (concat (a:as'))
    =                                           (concat.2)
        elem b (a ++ concat as')

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema ElemB: ¿elem w xs || elem w ys = elem w (xs ++ ys)?

    Demostración:
        Sea as' y bs' dos listas cualquiera (finitas y bien definidas), y c' un elemento. Por principio de inducción
        sobre la estructura as' es equivalente demostrar que:

        Caso base (as' = []):
            ¿elem c' [] || elem c' bs' = elem c' ([] ++ bs')?

        Caso inductivo (as' = (a':as'')):
            Hipotesis inductiva:
                ¡elem c' as'' || elem c' bs' = elem c' (as'' ++ bs')!

            Tesis inductiva:
                ¿elem c' (a':as'') || elem c' bs' = elem c' ((a':as'') ++ bs')?

        Demostración caso base:
            ¿elem c' [] || elem c' bs' = elem c' ([] ++ bs')?

        -- LADO IZQUIERDO

            elem c' [] || elem c' bs'
        =                                       (elem.1)
            False || elem c' bs'
        =                                       (||)
            elem c' bs'

        -- LADO DERECHO

            elem c' ([] ++ bs')
        =                                       ((++))
            elem c' ((++) [] bs')
        =                                       ((++))
            elem c' (bs')
        =                                       (aritmética)
            elem c' bs'
        
            -- Ambos lados llegan a lo mismo, el caso es válido.

        Demostración caso inductivo:
            ¿elem c' (a':as'') || elem c' bs' = elem c' ((a':as'') ++ bs')?

        -- LADO IZQUIERDO

            elem c' (a':as'') || elem c' bs'
        =                                       (elem.2)
            c' == a' || elem c' as'' || elem c' bs'
        =                                       (HI)
            c' == a' || elem c' (as'' ++ bs')

        -- LADO DERECHO

            elem c' ((a':as'') ++ bs')
        =                                       ((++))
            elem c' ((++) (a':as'') bs')
        =                                       ((++))
            elem c' (a' : ((++) as'' bs'))
        =                                       ((++))
            elem c' (a' : (as'' ++ bs'))
        =                                       (elem.2)
            c' == a' || elem c' (as'' ++ bs')

            -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.F

¿Para todo xs. para todo ys. subset xs ys = all (flip elem ys) xs?

Demostración:
    Sea as y bs listas cualquiera (finitas y bien definidas). Por principio de inducción sobre la 
    estructura as es equivalente demostrar que:

    Caso base (as = []):
        ¿subset [] bs = all (flip elem bs) []?

    Caso inductivo (as = (a:as')):
        Hipotesis inductiva:
            ¡subset as' bs = all (flip elem bs) as'!

        Tesis inductiva:
            ¿subset (a:as') bs = all (flip elem bs) (a:as')?

    Demostración caso base:
        ¿subset [] bs = all (flip elem bs) []?

    -- LADO IZQUIERDO

        subset [] bs
    =                           (subset.1)
        True

    -- LADO DERECHO

        all (flip elem bs) []
    =                           (all.1)
        True

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿subset (a:as') bs = all (flip elem bs) (a:as')?
    
    -- LADO IZQUIERDO

        subset (a:as') bs
    =                                               (subset.2)
        elem a bs && subset as' bs
    =                                               (HI)
        elem a bs && all (flip elem bs) as'

    -- LADO DERECHO

        all (flip elem bs) (a:as')
    =                                               (all.2)
        (flip elem bs) a && all (flip elem bs) as'
    =                                               (flip.1)
        elem a bs && all (flip elem bs) as'

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.G

all null = null . concat

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿para todo xs. all null xs = null . concat xs?

    Sea as una lista cualquiera (finita y bien definida). Por principio de inducción en la estructura
    as es equivalente demostrar que:

    Caso base (as = []):
        ¿all null [] = null . concat []?

    Caso inductivo (as = (a:as')):
        Hipotesis inductiva:
            ¡all null as' = null . concat as'!
        
        Tesis inductiva:
            ¿all null (a:as') = null . concat (a:as')?

    Demostración caso base:
        ¿all null [] = null . concat []?

    -- LADO IZQUIERDO

        all null []
    =                       (all.1)
        True

    -- LADO DERECHO

        null . concat []
    =                       (.)
        null (concat [])
    =                       (concat.1)
        null []
    =                       (null.1)
        True

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿all null (a:as') = null . concat (a:as')?
        
    -- LADO IZQUIERDO

        all null (a:as')
    =                                   (all.2)
        null a && all null as'
    =                                   (HI)
        null a && null . concat as'
    =                                   (.)
        null a && null (concat as')
    =                                   (Lema NullA)
        null (a ++ concat as')

    -- LADO DERECHO

        null . concat (a:as')
    =                                   (.)
        null (concat (a:as'))
    =                                   (concat.2)
        null (a ++ concat as')

        -- Ambos lados llegan a lo imsmo, el caso es válido y la propiedad también.

    Lema NullA: ¿null xs && null ys = null (xs ++ ys)?

    Demostración:
        Sea ws y zs dos listas cualquiera (finitas y bien definidas). Por principio de inducción 
        sobre la estructura de ws es equivalente demostrar que:

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
        =                           (&&)
            null zs

        -- LADO DERECHO

            null ([] ++ zs)
        =                           ((++))
            null ((++) [] zs)
        =                           ((++).1)
            null (zs)
        =                           (aritmética)
            null zs

            -- Ambos lados llegan a lo mismo, el caso es válido.

        Demostración caso inductivo:
            ¿null (w:ws') && null zs = null ((w:ws') ++ zs)?

        -- LADO IZQUIERDO

            null (w:ws') && null zs
        =                                   (null.2)
            False && null zs
        =                                   (&&)
            False

        -- LADO DERECHO

            null ((w:ws') ++ zs)
        =                                   ((++))
            null ((++) (w:ws') zs)
        =                                   ((++).2)
            null (w : ((++) ws' zs))
        =                                   ((++))
            null (w : (ws' ++ zs))
        =                                   (null.2)
            False

            -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.H

length = length . reverse

Demostración:
    Por principio de extensionalidad es equivalente demostrar que
    ¿para todo xs. length xs = length . reverse xs?

    Sea ws una lista cualquiera (finita y bien definida). Por principio de inducción
    sobre la estructura ws es equivalente demostrar que:

    Caso base (ws = []):
        ¿length [] = length . reverse []?

    Caso inductivo (ws = (w:ws')):
        Hipotesis inductiva:
            ¡length ws' = length . reverse ws'!

        Tesis inductiva:
            ¿length (w:ws') = length . reverse (w:ws')?

    Demostración caso base:
        ¿length [] = length . reverse []?

    -- LADO IZQUIERDO

        length []
    =                           (length.1)
        0

    -- LADO DERECHO

        length . reverse []
    =                           (.)
        length (reverse [])
    =                           (reverse.1)
        length []
    =                           (length.1)
        0

        -- Ambos lados llegan a lo mismo, la propiedad es válida.

    Demostración caso inductivo:
        ¿length (w:ws') = length . reverse (w:ws')?

    -- LADO IZQUIERDO

        length (w:ws')
    =                                   (length.2)
        1 + length ws'
    =                                   (HI)
        1 + length . reverse ws'
    =                                   (.)
        1 + length (reverse ws')


    -- LADO DERECHO

        length . reverse (w:ws')
    =                                           (.)
        length (reverse (w:ws'))
    =                                           (reverse.2)
        length (reverse ws' ++ [w])
    =                                           (Propiedad demostrada anteriormente en el 2.A)
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
    Sea as y bs listas cualquiera (finitas y bien definidas). Por principio de inducción 
    sobre la estructura as es equivalente demostrar que:

    Caso base (as = []):
        ¿reverse ([] ++ bs) = reverse bs ++ reverse []?

    Caso inductivo (as = (a:as')):
        Hipotesis inductiva:
            ¡reverse (as' ++ bs) = reverse bs ++ reverse as'!

        Tesis inductiva:
            ¿reverse ((a:as') ++ bs) = reverse bs ++ reverse (a:as')?

    Demostración caso base:
        ¿reverse ([] ++ bs) = reverse bs ++ reverse []?

    -- LADO IZQUIERDO

        reverse ([] ++ bs)
    =                               ((++))
        reverse ((++) [] bs)
    =                               ((++))
        reverse (bs)
    =                               (aritmética)
        reverse bs

    -- LADO DERECHO

        reverse bs ++ reverse []
    =                               (reverse.1)
        reverse bs ++ []
    =                               (Lema AppEmpty)
        reverse bs

        -- Ambos lados llegan a lo mismo, este caso es válido.

    Demostración caso inductivo:
        ¿reverse ((a:as') ++ bs) = reverse bs ++ reverse (a:as')?
    
    -- LADO IZQUIERDO

        reverse ((a:as') ++ bs)
    =                                       ((++))
        reverse ((++) (a:as') bs)
    =                                       ((++).2)
        reverse (a : ((++) as' bs))
    =                                       (reverse.2)
        reverse ((++) as' bs) ++ [a]
    =                                       ((++))
        reverse (as' ++ bs) ++ [a]

    -- LADO DERECHO

        reverse bs ++ reverse (a:as')
    =                                       (reverse.2)
        reverse bs ++ reverse as' ++ [a]
    =                                       (HI)
        reverse (as' ++ bs) ++ [a]

        -- Ambos lados llegan a lo mismo, este caso es válido y la propiedad también.

    Lema AppEmpty: ¿xs ++ [] = xs?

    Demostración:
        Sea ws una lista cualquiera (finita y bien definida). Por principio de inducción
        sobre la estructura ws es equivalente demostrar que:

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
        =               ((++))
            (++) [] []
        =               ((++).1)
            []

        -- LADO DERECHO

            []

            -- Ambos lados llegaron a lo mismo, el caso es válido.

        Demostración caso inductivo:
            ¿(w:ws') ++ [] = (w:ws')?

        -- LADO IZQUIERDO

            (w:ws') ++ []
        =                       ((++))
            (++) (w:ws') []
        =                       ((++).2)
            w : ((++) ws' [])
        =                       ((++))
            w : (ws' ++ [])
        =                       (HI)
            w : (ws')
        =                       (aritmética)
            (w:ws')

        -- LADO DERECHO

            (w:ws')

            -- Ambos lados llegaron a lo mismo, el caso es válido y la propiedad también.


-- 2.J

¿Para todo xs. para todo ys. all p (xs ++ ys) = all p (reverse xs) && all p (reverse ys)?

Demostración:
    Sea as y bs listas cualquiera (finitas y bien definidas). Por principio de inducción
    sobre la estructura as es equivalente a demostrar que:

    Caso base (as = []):
        ¿all p ([] ++ bs) = all p (reverse []) && all p (reverse bs)?

    Caso inductivo (as = (a:as')):
        Hipotesis inductiva:
            ¡all p (as' ++ bs) = all p (reverse as') && all p (reverse bs)!

        Tesis inductiva:
            ¿all p ((a:as') ++ bs) = all p (reverse (a:as')) && all p (reverse bs)?

    Demostración caso base:
        ¿all p ([] ++ bs) = all p (reverse []) && all p (reverse bs)?

    -- LADO IZQUIERDO

        all p ([] ++ bs)
    =                           ((++))
        all p ((++) [] bs)
    =                           ((++).1)
        all p (bs)
    =                           (aritmética)
        all p bs

    -- LADO DERECHO

        all p (reverse []) && all p (reverse bs)
    =                                               (reverse.1)
        all p [] && all p (reverse bs)
    =                                               (all.1)
        True && all p (reverse bs)
    =                                               (&&)
        all p (reverse bs)
    =                                               (Lema AllReverse)
        all p bs

        -- Ambos lados llegan a lo mismo, el caso es válido.

    Demostración caso inductivo:
        ¿all p ((a:as') ++ bs) = all p (reverse (a:as')) && all p (reverse bs)?

    -- LADO IZQUIERDO

        all p ((a:as') ++ bs)
    =                                                       ((++))
        all p ((++) (a:as') bs)
    =                                                       ((++).2)
        all p (a : ((++) as' bs))
    =                                                       (all.2)
        p a && all p ((++) as' bs)
    =                                                       ((++))
        p a && all p (as' ++ bs)
    =                                                       (HI)
        p a && all p (reverse as') && all p (reverse bs)
    =                                                       (Lema AllReverse)
        p a && all p as' && all p (reverse bs)

    -- LADO DERECHO

        all p (reverse (a:as')) && all p (reverse bs)
    =                                                       (Lema AllReverse)
        all p (a:as') && all p (reverse bs)
    =                                                       (all.2)
        p a && all p as' && all p (reverse bs)

        -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.

    Lema AllReverse: ¿all p xs = all p (reverse xs)?

    Demostración:
        Sea ws una lista cualquiera (finita y bien definida). Por principio de inducción
        sobre la estructura ws es equivalente demostrar que:

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
        =                        (all.1)
            True

        -- LADO DERECHO

            all p (reverse [])
        =                       (reverse.1)
            all p ([])
        =                       (aritmética)
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
        =                                       (&&)
            all p ws' && p w
        =                                       (aritmética)
            p w && all p ws'

            -- Ambos lados legan a lo mismo, el caso es válido y la propiedad también.

    Lema AllDist: ¿all p (xs ++ ys) = all p xs && all p ys?

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
        =                           ((++))
            all p ((++) [] js)
        =                           ((++).1)
            all p (js)
        =                           (aritmética)
            all p js

        -- LADO DERECHO

            all p [] && all p js
        =                           (all.1)
            True && all p js
        =                           (&&)
            all p js

            -- Ambos lados llegan a lo mismo, el caso es válido.

        Demostración caso inductivo:
            ¿all p ((k:ks') ++ js) = all p (k:ks') && all p js?

        -- LADO IZQUIERDO

            all p ((k:ks') ++ js)
        =                                   ((++))
            all p ((++) (k:ks') js)
        =                                   ((++).2)
            all p (k : ((++) ks' js))
        =                                   (all.2)
            p k && all p ((++) ks' js)
        =                                   ((++))
            p k && all p (ks' ++ js)
        =                                   (HI)
            p k && all p ks' && all p js

        -- LADO DERECHO

            all p (k:ks') && all p js
        =                                   (all.2)
            p k && all p ks' && all p js

            -- Ambos lados llegan a lo mismo, el caso es válido y la propiedad también.


-- 2.K

¿Para todo xs. para todo ys. unzip (zip xs ys) = (xs, ys)?  (En este caso, mostrar que no vale...)

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
prodN (S n) m = addN (prodN n m) m


-- 1.A.IV

int2N  ::  Int  ->  N
int2N 0 = Z
int2N n = S (int2N (n-1))


-- 1.B.I

¿Para todo n1. para todo n2. evalN (addN n1 n2) = evalN n1 + evalN n2?

Demostración:
    Sea n y m dos elementos N. Por principio de inducción sobre la estructura n
    es equivalente demostrar que:

    Caso base (n = Z):
        ¿evalN (addN Z m) = evalN Z + evalN m?

    Caso inductivo (n = (S n')):
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




-- 1.B.III

int2N . evalN = id

Demostración:




-- 1.B.IV

evalN . int2N = id 

Demostración:




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


> Ejercicio 3:

type NBin = [DigBin]

-- 3.A.I:


-- 3.A.II:


-- 3.A.III:


-- 3.A.IV:


-- 3.A.V:


-- 3.A.VI:



-- 3.B.I:


-- 3.B.II:


-- 3.B.III:


-- 3.B.IV:


-- 3.B.V:


-- 3.C.I:


-- 3.C.II:


> Ejercicio 4:

-- 4.A.I:


-- 4.A.II:

-- 4.A.III:


-- 4.A.IV:


-- 4.A.V:


-- 4.A.VI:


-- 4.B.I:


-- 4.B.II:


-- 4.B.III:


-- 4.B.IV:


> Ejercicio 5:



# SECCIÓN 3

> Ejercicio 1: