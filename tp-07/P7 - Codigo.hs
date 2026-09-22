--------------------------------------------------------------------------------------------------------

--                      TRABAJO PRÁCTICO N°7: Inducción y Recursión Estructural I                     --

--------------------------------------------------------------------------------------------------------

## SECCIÓN 1

data Pizza = Prepizza | Capa Ingrediente Pizza 

data Ingrediente = Aceitunas Int | Anchoas | Cebolla 
                 | Jamón         | Queso   | Salsa


> Ejercicio 1:

Reglas que definen el conjunto Pizza:
    > Regla base: Prepizza tiene que estar en el conjunto Pizza.

    > Regla inductiva: Sea que i está en el conjunto Ingrediente, y p pertenece al conjunto Pizza, entonces
                       Capa i p pertenece al conjunto Pizza.


> Ejercicio 2:

f :: Pizza -> ...
f Prepizza   = ...
f (Capa i p) = ... i ... f p


> Ejercicio 3:

-- 3.A

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p 


-- 3.B

cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza   = 0
cantidadDeAceitunas (Capa i p) = cantAceitunas i + cantidadDeAceitunas p

cantAceitunas :: Ingrediente -> Int
cantAceitunas (Aceitunas n) = n
cantAceitunas _             = 0


-- 3.C

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = Capa (duplicarAc i) (duplicarAceitunas p)

duplicarAc :: Ingrediente -> Ingrediente
duplicarAc (Aceitunas n) = (Aceitunas (n * 2))
duplicarAc i             = i


-- 3.D

sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza   = Prepizza
sinLactosa (Capa i p) = capaSinLactosa i (sinLactosa p)

capaSinLactosa :: Ingrediente -> Pizza -> Pizza
capaSinLactosa Queso p = p
capaSinLactosa i     p = Capa i p 


-- 3.E

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza   = True
aptaIntolerantesLactosa (Capa i p) = esIntoleranteLactosa i && aptaIntoleranteLactosa p

esIntoleranteLactosa :: Ingrediente -> Bool
esIntoleranteLactosa Queso = False
esIntoleranteLactosa _     = True


-- 3.F

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza   = Prepizza
conDescripcionMejorada (Capa i p) = conDescM i (conDescripcionMejorada p)

conDescM :: Ingrediente -> Pizza -> Pizza
conDescM (Aceitunas n1) (Capa (Aceitunas n2) p) = Capa (Aceituna (n1 + n2)) p 
conDescM i              p                       = Capa i p


> Ejercicio 4:

-- 4.A

cantidadDeAceitunas Prepizza = cantidadDeAceitunas(conDescripcionMejorada Prepizza)


-- 4.B

cantidadDeAceitunas (Capa Queso Prepizza) = cantidadDeAceitunas (conDescripcionMejorada (Capa Queso Prepizza))


-- 4.C

cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza)) =
cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza)))




-- 4.D

cantidadDeAceitunas (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))) =
cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))))



## Sección 2

> Ejercicio 1:



> Ejercicio 2:


