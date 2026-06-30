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
    where (Capa i p) = fc i (go p)
          Prepizza   = fp

pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada fc fp Prepizza   = fp
pizzaProcesada fc fp (Capa i p) = fc i (pizzaProcesada fc fp p)


> Ejercicio 4:

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen f = pizzaProcesada (\ i p' -> unoSi (f i) + p') 0

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen = (\ f -> pizzaProcesada (\ i p' -> unoSi (f i) + p') 0)


conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas f = pizzaProcesada (\ i p' -> Capa (f i) p') Prepizza

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas = (\ f -> pizzaProcesada (\ i p' -> Capa (f i) p') Prepizza)


soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue f = pizzaProcesada (\ i p' -> capaSiCumple f i p') Prepizza

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue = (\ f -> pizzaProcesada (\ i p' -> capaSiCumple f i p') Prepizza)


sinLactosa :: Pizza -> Pizza
sinLactosa = pizzaProcesada (\ i p' -> capaSiCumple f i p') Prepizza
    where f = (\ i -> not (esQueso i))

sinLactosa :: Pizza -> Pizza
sinLactosa = pizzaProcesada (\ i p' -> (\ f i p' -> if f i then Capa i p' else p') (\ i -> not (esQueso i)) i p') Prepizza


aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa = pizzaProcesada (\ i p' -> esQueso i && p') False


cantidadDeQueso :: Pizza -> Int
cantidadDeQueso = pizzaProcesada (\ i p' -> unoSi (esQueso i) + p') 0


conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = pizzaProcesada (\ i p' -> Capa (duplicarAceitunas i) p') Prepizza