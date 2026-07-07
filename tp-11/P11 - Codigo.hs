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
cantidadCapasQueCumplen f (Capa i p) = unoSi (f i) + (cantidadCapasQueCumplen f p)

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
soloLasCapasQue f (Capa i p) = capaSiCumple f i (soloLasCapasQue f p)

capaSiCumple :: (Ingrediente -> Bool) -> Ingrediente -> Pizza -> Pizza
capaSiCumple f i p = if f i
                        then Capa i p
                        else p


> Ejercicio 2:

-- 2.A

sinLactosa :: Pizza -> Pizza
sinLactosa p = soloLasCapasQue (\i -> not (esQueso i)) p

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False


-- 2.B

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa p = cantidadCapasQueCumplen (\i -> esQueso i) p == 0


-- 2.C

cantidadDeQueso :: Pizza -> Int
cantidadDeQueso p = cantidadCapasQueCumplen (\i -> esQueso i) p


-- 2.D

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas p = conCapasTransformadas (\i -> duplicarAceitunas i) p

duplicarAceitunas :: Ingrediente -> Ingrediente
duplicarAceitunas (Aceitunas n) = Aceitunas (n*2)
duplicarAceitunas i             = i


> Ejercicio 3:

pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada fc fp = go
    where go (Capa i p) = fc i (go p)
          go Prepizza   = fp

pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada fc fp Prepizza   = fp
pizzaProcesada fc fp (Capa i p) = fc i (pizzaProcesada fc fp p)


> Ejercicio 4:

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen f = pizzaProcesada (\i p' -> unoSi (f i) + p') 0

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen = (\f -> pizzaProcesada (\i p' -> unoSi (f i) + p') 0)



conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas f = pizzaProcesada (\i p' -> Capa (f i) p') Prepizza

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas = (\f -> pizzaProcesada (\i p' -> Capa (f i) p') Prepizza)



soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue f = pizzaProcesada (\i p' -> capaSiCumple f i p') Prepizza

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue = (\f -> pizzaProcesada (\i p' -> capaSiCumple f i p') Prepizza)



sinLactosa :: Pizza -> Pizza
sinLactosa = pizzaProcesada (\i p' -> capaSiCumple f i p') Prepizza
    where f = (\i -> not (esQueso i))

sinLactosa :: Pizza -> Pizza
sinLactosa = pizzaProcesada (\i p' -> (\f i p' -> if f i then Capa i p' else p') (\i -> not (esQueso i)) i p') Prepizza



aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa = pizzaProcesada (\i p' -> esQueso i && p') False



cantidadDeQueso :: Pizza -> Int
cantidadDeQueso = pizzaProcesada (\i p' -> unoSi (esQueso i) + p') 0



conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = pizzaProcesada (\i p' -> Capa (duplicarAceitunas i) p') Prepizza


> Ejercicio 5:

-- 5.A

cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = pizzaProcesada (\i p' -> cantidadAc i + p') 0

cantidadAc :: Ingrediente -> Int
cantidadAc (Aceitunas n) = n
cantidadAc _             = 0


-- 5.B

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen f = pizzaProcesada (\i p' -> if f i then i : p' else p') []


-- 5.C

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada = pizzaProcesada (\i p' -> descripcionMejorada i p') Prepizza

descripcionMejorada :: Ingrediente -> Pizza -> Pizza
descripcionMejorada (Aceitunas n1) (Capa (Aceitunas n2) p) = Capa (Aceitunas (n1 + n2)) p
descripcionMejorada i              p                       = Capa i p


-- 5.D

conCapasDe :: Pizza -> Pizza -> Pizza
conCapasDe p1 = pizzaProcesada (\i p' -> Capa i p') p1


-- 5.E

primerasNCapas :: Int -> Pizza -> Pizza
primerasNCapas n p = pizzaProcesada (\i p' n' -> if n' == 0 then Prepizza else Capa i (p' (n' - 1))) (\_ -> Prepizza) p n

primerasNCapas :: Int -> Pizza -> Pizza
primerasNCapas = flip (pizzaProcesada (\i p' n' -> if n' == 0 then Prepizza else Capa i (p' (n' - 1))) (\_ -> Prepizza))


> Ejercicio 6:

-- 6.A

Para todo f. length . capasQueCumplen f = cantidadDe f



-- 6.B

Para todo f. para todo p1. para todo p2.
cantidadCapasQueCumplen f (conCapasDe p1 p2) = cantidadCapasQueCumplen f p1 + cantidadCapasQueCumplen f p2



-- 6.C

Para todo f. para todo p1. para todo p2.
conCapasTransformadas f (conCapasDe p1 p2) = conCapasDe (conCapasTransformadas f p1) (conCapasTransformadas f p2)



-- 6.D

Para todo f. cantidadCapasQueCumplen f . soloLasCapasQue f = cantidadCapasQueCumplen f



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
