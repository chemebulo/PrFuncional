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
cantidadCapasQueCumplen f

-- 4.B

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza 
conCapasTransformadas f

-- 4.C

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue f

-- 4.D

sinLactosa :: Pizza -> Pizza 
sinLactosa

-- 4.E

aptaIntolerantesLactosa :: Pizza -> Bool 
aptaIntolerantesLactosa

-- 4.F

cantidadDeQueso :: Pizza -> Int 
cantidadDeQueso

-- 4.G

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas


> Ejercicio 5:



> Ejercicio 6:

-- 6.A

map :: (a -> b) -> [a] -> [b] 

-- 6.B

filter :: (a -> Bool) -> [a] -> [a] 

-- 6.C

foldr :: (a -> b -> b) -> b -> [a] -> b 

-- 6.D

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b 

-- 6.E

foldr1 :: (a -> a -> a) -> [a] -> a 

-- 6.F

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 

-- 6.G

scanr :: (a -> b -> b) -> b -> [a] -> [b]


> Ejercicio 7:



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
