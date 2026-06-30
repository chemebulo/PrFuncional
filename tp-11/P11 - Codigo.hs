--------------------------------------------------------------------------------------------------------

--                              TRABAJO PRÁCTICO N°11: Esquemas de Funciones I                        --

--------------------------------------------------------------------------------------------------------

## SECCIÓN 1

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
    where 