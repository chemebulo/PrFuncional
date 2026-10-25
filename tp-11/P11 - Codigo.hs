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

-- 7.B

filter :: (a -> Bool) -> [a] -> [a] 

-- 7.C

foldr :: (a -> b -> b) -> b -> [a] -> b 

-- 7.D

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b 

-- 7.E

foldr1 :: (a -> a -> a) -> [a] -> a 

-- 7.F

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 

-- 7.G

scanr :: (a -> b -> b) -> b -> [a] -> [b]


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

-- 9.B

length :: a -> Int

-- 9.C

map :: (a -> b) -> [a] -> [b]

-- 9.D

filter :: (a -> Bool) -> [a] -> [a]

-- 9.E

find :: (a -> Bool) -> [a] -> Maybe a

-- 9.F

any :: (a -> Bool) -> [a] -> Bool

-- 9.G

all :: (a -> Bool) -> [a] -> Bool

-- 9.H

countBy :: (a -> Bool) -> [a] -> Int

-- 9.I

partition :: (a -> Bool) -> [a] -> ([a], [a])

-- 9.J

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

-- 9.K

scanr :: (a -> b -> b) -> b -> [a] -> [b]

-- 9.L

takeWhile :: (a -> Bool) -> [a] -> [a]

-- 9.M

take :: Int -> [a] -> [a]

-- 9.N

drop :: Int -> [a] -> [a]

-- 9.O

elemAt :: Int -> [a] -> a


> Ejercicio 10:
