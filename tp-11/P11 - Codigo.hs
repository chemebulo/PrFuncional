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




-- 6.B




-- 6.C




-- 6.D




> Ejercicio 7:


-- 7.A




-- 7.B




-- 7.C




-- 7.D




-- 7.E




-- 7.F




-- 7.G




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




-- 9.B




-- 9.C




-- 9.D




-- 9.E




-- 9.F




-- 9.G




-- 9.H




-- 9.I




-- 9.J




-- 9.K




-- 9.L




-- 9.M




-- 9.N




-- 9.O

