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
(++) (x:xs) ys = x : (++) xs ys


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

    Caso inductivo (zs = z:zs'):
        Hipotesis inductiva:
            ¡length (zs' ++ ws) = length zs' + length ws!
        
        Tesis inductiva:
            ¿length ((z:zs') ++ ws) = length (z:zs') + length ws?

    Demostración caso base:
        ¿length ([] ++ ws) = length [] + length ws?

    -- LADO IZQUIERDO

        length ([] ++ ws)
    =                               (++.1)
        length ws

    -- LADO DERECHO

        length [] + length ws
    =                               (length.1)
        0 + length ws
    =                               (aritmética)
        length ws

    -- Ambos lados llegan a lo mismo, este caso es válido.

    Demostración caso inductivo:
        ¿length ((z:zs') ++ ws) = length (z:zs') + length ws?

    -- LADO IZQUIERDO

        length ((z:zs') ++ ws)
    =                               ((++))
        length ((++) (z:zs') ws)
    =                               ((++).2)
        length (z : (++) zs' ws)
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

    -- Ambos lados llegan a lo mismo, este caso es válido y la propiedad también.


-- 2.B

¿Para todo xs. para todo ys. para todo zs. (xs ++ ys) ++ zs = xs ++ (ys ++ zs)?

Demostración:
    Sea as, bs y cs listas cualquiera (finitas y bien definidas). Por principio de inducción en la estructura
    de as es equivalente demostrar:

    Caso base (as = []):
        ¿([] ++ bs) ++ cs = [] ++ (bs ++ cs)?

    Caso inductivo (as = a:as'):
        Hipotesis inductiva:
            ¡(as' ++ bs) ++ cs = as' ++ (bs ++ cs)!

        Tesis inductiva:
            ¿((a:as') ++ bs) ++ cs = (a:as') ++ (bs ++ cs)?

    Demostración caso base:
        