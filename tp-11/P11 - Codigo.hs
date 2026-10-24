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
cantidadCapasQueCumplen Prepizza   =
cantidadCapasQueCumplen (Capa i p) = 

-- 1.B

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza 
conCapasTransformadas Prepizza   = 
conCapasTransformadas (Capa i p) =

-- 1.C

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue Prepizza   = 
soloLasCapasQue (Capa i p) =


> Ejercicio 2:






> Ejercicio 3:




> Ejercicio 4:



> Ejercicio 5:



> Ejercicio 6:


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
