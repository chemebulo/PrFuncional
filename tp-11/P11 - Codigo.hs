--------------------------------------------------------------------------------------------------------

--                              TRABAJO PRÁCTICO N°11: Esquemas de Funciones I                        --

--------------------------------------------------------------------------------------------------------

> Ejercicio 1:

data Ingrediente = Aceitunas Int | Jamon | Queso | Salsa
    deriving Show

data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show

-- 1.A

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int 
cantidadCapasQueCumplen f Prepizza   = 0
cantidadCapasQueCumplen f (Capa i p) = unoSi (f i) + cantidadCapasQueCumplen f p

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

-- 1.B

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza 
conCapasTransformadas f Prepizza   = Prepizza
conCapasTransformadas f (Capa i p) = Capa (f i) (conCapasTransformadas f p)

-- 1.C

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue f Prepizza   = Prepizza
soloLasCapasQue f (Capa i p) = if f i
                                  then Capa i (soloLasCapasQue f p)
                                  else soloLasCapasQue f p


> Ejercicio 2:

-- 2.A

sinLactosa :: Pizza -> Pizza 
sinLactosa p = soloLasCapasQue (\i -> not (esQueso i)) p

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

-- 2.B

aptaIntolerantesLactosa :: Pizza -> Bool 
aptaIntolerantesLactosa p = cantidadDeQueso p == 0

-- 2.C

cantidadDeQueso :: Pizza -> Int 
cantidadDeQueso p = cantidadCapasQueCumplen (\i -> esQueso i) p

-- 2.D

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas p = conCapasTransformadas (\i -> dobleAceitunas i) p

dobleAceitunas :: Ingrediente -> Ingrediente
dobleAceitunas (Aceitunas n) = Aceitunas (n * 2)
dobleAceitunas i             = i


> Ejercicio 3:

pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada f x Prepizza   = x
pizzaProcesada f x (Capa i p) = f i (pizzaProcesada f x p)


pizzaProcesadaV2 :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesadaV2 fc fp = go
    where go Prepizza   = fp 
          go (Capa i p) = fc i (go p)


> Ejercicio 4:

-- 4.A

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int 
cantidadCapasQueCumplen f = pizzaProcesada (\i p' -> unoSi (f i) + p') 0

cantidadCapasQueCumplenV2 :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplenV2 = (\f -> pizzaProcesada (\i p' -> unoSi (f i) + p') 0)

-- 4.B

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza 
conCapasTransformadas f = pizzaProcesada (\i p' -> Capa (f i) p') Prepizza

conCapasTransformadasV2 :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza 
conCapasTransformadasV2 = (\f -> pizzaProcesada (\i p' -> Capa (f i) p') Prepizza)

-- 4.C

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue f = pizzaProcesada (\i p' -> if f i then Capa i p' else p') Prepizza

soloLasCapasQueV2 :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQueV2 = (\f -> pizzaProcesada (\i p' -> if f i then Capa i p' else p') Prepizza)

-- 4.D

sinLactosa :: Pizza -> Pizza 
sinLactosa = pizzaProcesada (\i p' -> if esQueso i then p' else Capa i p') Prepizza

-- 4.E

aptaIntolerantesLactosa :: Pizza -> Bool 
aptaIntolerantesLactosa = pizzaProcesada (\i p' -> not (esQueso i) && p') True

-- 4.F

cantidadDeQueso :: Pizza -> Int 
cantidadDeQueso = pizzaProcesada (\i p' -> unoSi (esQueso i) + p') 0

-- 4.G

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = pizzaProcesada (\i p' -> Capa (dobleAceitunas i) p') Prepizza


> Ejercicio 5:

-- 5.A

cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = pizzaProcesada (\i p' -> cantidadAc i + p') 0
    where cantidadAc (Aceitunas n) = n
          cantidadAc _             = 0

-- 5.B

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen f = pizzaProcesada (\i is -> if f i then (i : is) else is ) []

capasQueCumplenV2 :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplenV2 = (\f -> pizzaProcesada (\i is -> if f i then (i : is) else is ) [])
    
-- 5.C

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada = pizzaProcesada (\i p' -> descMejorada i p') Prepizza
    where descMejorada (Aceitunas n) (Capa (Aceitunas n') p) = Capa (Aceitunas (n + n')) p
          descMejorada i             p                       = Capa i p

-- 5.D

conCapasDe :: Pizza -> Pizza -> Pizza
conCapasDe = (\p2 -> flip (pizzaProcesada (\i p' -> Capa i p')) p2)

-- 5.E

primerasNCapas :: Int -> Pizza -> Pizza
primerasNCapas = flip (pizzaProcesada ctk (\n -> Prepizza))
    where ctk i p' = (\n -> if == 0 then Prepizza else Capa i (p' (n-1)))

primerasNCapasV2 :: Int -> Pizza -> Pizza
primerasNCapasV2 = flip (pizzaProcesada (\i p' -> (\n -> if n == 0 then Prepizza else Capa i (p' (n-1)))) (\n -> Prepizza))


> Ejercicio 6:










> Ejercicio 7:

-- 7.A

map :: (a -> b) -> [a] -> [b] 
map f []     = []
map f (x:xs) = f x : map f xs 

-- 7.B

filter :: (a -> Bool) -> [a] -> [a] 
filter p []     = []
filter p (x:xs) = if p x then x : filter p xs
                         else filter p xs

-- 7.C

foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

-- 7.D

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b 
recr z f []     = z
recr z f (x:xs) = f x xs (recr z f xs)

-- 7.E

foldr1 :: (a -> a -> a) -> [a] -> a 
foldr1 f []     = error "No existen elementos en la lista"
foldr1 f [x]    = x
foldr1 f (x:xs) = f x (foldr1 f xs)

-- 7.F

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith f _      _      = []

-- 7.G

scanr :: (a -> b -> b) -> b -> [a] -> [b]
-- COSTO: O(n).
scanr _ z []     = [z]
scanr f z (x:xs) = let (y:ys) = scanr f z xs
					 in f x y : (y:ys)

scanrV2 :: (a -> b -> b) -> b -> [a] -> [b]
-- COSTO: O(n).
scanrV2 _ z []     = [z]
scanrV2 f z (x:xs) = case (scanrV2 f z xs) of
                        (y:ys) -> f x y : (y:ys)

scanrV3 :: (a -> b -> b) -> b -> [a] -> [b]
-- COSTO: O(n^2).
scanrV3 f z []     = [z]
scanrV3 f z (x:xs) = f x (foldr f z xs) : (scanrV3 f z xs)


> Ejercicio 8:

-- 8.A




-- 8.B




-- 8.C





-- 8.D





-- 8.E





-- 8.F





-- 8.G





-- 8.H





-- 8.I





-- 8.J





-- 8.K




> Ejercicio 9:

-- 9.A

sum :: [Int] -> Int
sum = foldr (+) 0

-- 9.B

length :: [a] -> Int
length = foldr (\_ xs' -> 1 + xs') 0 

-- 9.C

map :: (a -> b) -> [a] -> [b]
map = (\f -> foldr (\x xs' -> f x : xs') [])

-- 9.D

filter :: (a -> Bool) -> [a] -> [a]
filter = (\p -> foldr (\x xs' -> if p x then x : xs' else xs') [])

-- 9.E

find :: (a -> Bool) -> [a] -> Maybe a
find = (\p -> foldr (\x xs' -> if p x then Just x else xs') Nothing)

-- 9.F

any :: (a -> Bool) -> [a] -> Bool
any = (\f -> foldr (\x xs' -> f x || xs') False)

-- 9.G

all :: (a -> Bool) -> [a] -> Bool
all = (\f -> foldr (\x xs' -> f x && xs') True)

-- 9.H

countBy :: (a -> Bool) -> [a] -> Int
countBy = (\f -> foldr (\x xs' -> if f x then 1 + xs' else xs') 0)

-- 9.I

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition = (\f -> foldr (\x xs' -> 
                let (xs1, xs2) in if f x then (x:xs1, xs2) else (xs1, x:xs2))
                ([], [])
            )

-- 9.J

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f = foldr ctk (\_ -> [])
    where ctk x xs' = \ys -> case ys of
                                (y:ys') -> f x y : xs' ys' 
                                []      -> []

-- 9.K

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr = (\f z -> foldr (\x xs' -> let (y:ys) = xs' in f x y : xs') [z])

-- 9.L

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = (\f -> foldr (\x xs' -> if f x then x : xs' else []) [])

-- 9.M

take :: Int -> [a] -> [a]
take = flip (foldr ctk (\n -> []))
    where ctk x xs' = (\n -> if n == 0 then [] else x : xs' (n-1))

-- 9.N

drop :: Int -> [a] -> [a]
drop = flip (foldr ctk (\n -> []))
    where ctk x xs' = (\n -> if n == 0 then x : xs' n else xs' (n-1))

-- 9.O

elemAt :: Int -> [a] -> a
elemAt = flip (foldr ctk (\n -> error "No existe esa posición en la lista"))
    where ctk x xs' = (\n -> if n == 0 then x else xs' (n-1))


> Ejercicio 10:

-- 10.A

filter id :: [Bool] -> [Bool]

-- 10.B

map (\x y z -> (x, y, z)) :: [a] -> [b -> c -> (a, b, c)]

-- 10.C

map (+) :: [Int] -> [Int -> Int]

-- 10.D

filter fst :: [(Bool, a)] -> [(Bool, a)]

-- 10.E

filter (flip const (+)) :: [Bool] -> [Bool]

-- 10.F

map const :: [a] -> [b -> a]

-- 10.G

map twice :: [a -> a] -> [a -> a]

-- 10.H

foldr twice :: a -> [a -> a] -> a

-- 10.I

zipWith fst :: [(a -> b, c)] -> [a] -> [b]

-- 10.J

foldr (\x r z -> (x, z) : r z) (const []) :: [a] -> b -> [(a, b)]