--------------------------------------------------------------------------------------------------------

--                      TRABAJO PRÁCTICO N°7: Inducción y Recursión Estructural I                     --

--------------------------------------------------------------------------------------------------------

## SECCIÓN 1

data Pizza = Prepizza | Capa Ingrediente Pizza 

data Ingrediente = Aceitunas Int | Anchoas | Cebolla 
                 | Jamón         | Queso   | Salsa


> Ejercicio 1:

Reglas que definen el conjunto Pizza:
    > Regla base: Prepizza está en el conjunto Pizza.

    > Regla inductiva: Si i está en el conjunto Ingrediente, y p está en el conjunto Pizza, entonces
                       Capa i p está en el conjunto Pizza.


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
conDescripcionMejorada (Capa i p) = juntarAc i (conDescripcionMejorada p)

juntarAc :: Ingrediente -> Pizza -> Pizza
juntarAc (Aceitunas n1) (Capa (Aceitunas n2) p) = Capa (Aceituna (n1 + n2)) p 
juntarAc i              p                       = Capa i p


> Ejercicio 4:

-- 4.A

cantAcs Pp = cantAcs (conDescM Pp)

Demostración:

    -- LADO IZQUIERDO

        cantAcs Pp

    -- LADO DERECHO

        cantAcs (conDescM Pp)
    =                                       (conDescM.1)
        cantAcs Pp

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.B

cantAcs (C Q Pp) = cantAcs (conDescM (C Q Pp))

Demostración:

    -- LADO IZQUIERDO

        cantAcs (C Q Pp)

    -- LADO DERECHO

        cantAcs (conDescM (C Q Pp))
    =                                                   (conDescM.2)
        cantAcs (juntarAc Q (conDescM Pp))
    =                                                   (conDescM.1)
        cantAcs (juntarAc Q Pp)
    =                                                   (juntarAc.2)
        cantAcs (C Q Pp)

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.C

cantAcs (C (Ac 8) (C Q Pp)) = cantAcs (conDescM (C (Ac 8) (C Q Pp)))

Demostración:

    -- LADO IZQUIERDO

        cantAcs (C (Ac 8) (C Q Pp))

    -- LADO DERECHO

        cantAcs (conDescM (C (Ac 8) (C Q Pp)))
    =                                                           (conDescM.2)
        cantAcs (juntarAc (Ac 8) (conDescM (C Q Pp)))
    =                                                           (conDescM.2)
        cantAcs (juntarAc (Ac 8) (juntarAc Q (conDescM Pp)))
    =                                                           (conDescM.1)
        cantAcs (juntarAc (Ac 8) (juntarAc Q Pp))
    =                                                           (juntarAc.2)
        cantAcs (juntarAc (Ac 8) (C Q Pp))
    =                                                           (juntarAc.2)
        cantAcs (C (Ac 8) (C Q Pp))

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.D

cantAcs (C (Ac 9) (C (Ac 8) (C Q Pp))) = cantAcs (conDescM (C (Ac 9) (C (Ac 8) (C Q Pp))))

Demostración:

    -- LADO IZQUIERDO

        cantAcs (C (Ac 9) (C (Ac 8) (C Q Pp)))
    =                                                                   (cantAcs.2)
        cantAceitunas (Ac 9) + cantAcs (C (Ac 8) (C Q Pp))
    =                                                                   (cantAcs.2)
        cantAceitunas (Ac 9) + cantAceitunas (Ac 8) + cantAcs (C Q Pp)
    =                                                                   (cantAceitunas.1)
        9 + cantAceitunas (Ac 8) + cantAcs (C Q Pp)
    =                                                                   (cantAceitunas.1)
        9 + 8 + cantAcs (C Q Pp)
    =                                                                   (aritmética)
        17 + cantAcs (C Q Pp)

    -- LADO DERECHO

        cantAcs (conDescM (C (Ac 9) (C (Ac 8) (C Q Pp))))
    =                                                                   (conDescM.2)
        cantAcs (juntarAc (Ac 9) (conDescM (C (Ac 8) (C Q Pp))))
    =                                                                   (conDescM.2)
        cantAcs (juntarAc (Ac 9) (juntarAc (Ac 8) (C Q Pp)))
    =                                                                   (juntarAc.2)
        cantAcs (juntarAc (Ac 9) (C (Ac 8) (C Q Pp)))
    =                                                                   (juntarAc.1)
        cantAcs (C (Ac 17) (C Q Pp))
    =                                                                   (cantAcs.2)
        cantAceitunas (Ac 17) + cantAcs (C Q Pp)
    =                                                                   (cantAceitunas.1)
        17 + cantAcs (C Q Pp)

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


## Sección 2

> Ejercicio 1:



> Ejercicio 2:


