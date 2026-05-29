--------------------------------------------------------------------------------------------------------

--                      TRABAJO PRÁCTICO N°7: Inducción y Recursión Estructural                       --

--------------------------------------------------------------------------------------------------------

## SECCIÓN 1

data Pizza = Prepizza | Capa Ingrediente Pizza 

data Ingrediente = Aceitunas Int | Anchoas | Cebolla 
                 | Jamón         | Queso   | Salsa


> Ejercicio 1:

PIZZA:
    -- Aseguramos que el caso base exista en el conjunto.
    Regla base: Prepizza existe en el conjunto Pizza.
    -- Aseguramos que el caso inductivo exista en el conjunto. 
    Regla inductiva: Sea que i pertenece al conjunto Ingrediente, y p pertenece al conjunto Pizza, entonces
                     Capa i p pertenece al conjunto Pizza.


> Ejercicio 2:

f :: Pizza -> ...
f Prepizza   = ...
f (Capa i p) = ... i ... f p


> Ejercicio 3:

-- 3.A

cantidadDeCapas :: Pizza -> Int
p :: Pizza
--------------------------------
cantidadDeCapas p :: Int

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p


-- 3.B

cantidadDeAceitunas :: Pizza -> Int
p :: Pizza
-----------------------------------
cantidadDeAceitunas p :: Int

cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza   = 0
cantidadDeAceitunas (Capa i p) = cantAceitunas i + cantidadDeAceitunas p

cantAceitunas :: Ingrediente -> Int
cantAceitunas (Aceitunas n) = n
cantAceitunas _             = 0


-- 3.C

duplicarAceitunas :: Pizza -> Pizza
p :: Pizza
-----------------------------------
duplicarAceitunas p :: Pizza

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = Capa (duplicarAc i) (duplicarAceitunas p)

duplicarAc :: Ingrediente -> Ingrediente
duplicarAc (Aceitunas n) = (Aceitunas (n*2))
duplicarAc i             = i

-- ¿p​. cantidadDeAceitunas (duplicarAceitunas ​p​) ​=​ 2 * cantidadDeAceitunas ​p?
-- ¿Se puede demostrar sin un ejemplo concreto? Sí, pero con el principio de inducción estructural. 


-- 3.D

sinLactosa :: Pizza -> Pizza
p :: Pizza
----------------------------
sinLactosa p :: Pizza

sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza   = Prepizza
sinLactosa (Capa i p) = capaSinLactosa i (sinLactosa p)

capaSinLactosa :: Ingrediente -> Pizza -> Pizza
capaSinLactosa Queso p = p
capaSinLactosa i     p = Capa i p


-- 3.E

aptaIntoleranteLactosa :: Pizza -> Bool
p :: Pizza
---------------------------------------
aptaIntoleranteLactosa p :: Bool

aptaIntoleranteLactosa :: Pizza -> Bool
aptaIntoleranteLactosa Prepizza   = True
aptaIntoleranteLactosa (Capa i p) = sinLactosaIng i && aptaIntoleranteLactosa p

sinLactosaIng :: Ingrediente -> Bool
sinLactosaIng Queso = False
sinLactosaIng _     = True


-- 3.F

conDescripcionMejorada :: Pizza -> Pizza
p :: Pizza
---------------------------------------
conDescripcionMejorada p :: Pizza

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza   = Prepizza
conDescripcionMejorada (Capa i p) = juntarAc i (conDescripcionMejorada p)

juntarAc :: Ingrediente -> Pizza -> Pizza
juntarAc (Aceituna n) (Capa (Aceitunas n') p) = Capa (Aceitunas (n+n')) p
juntarAc i            p                       = Capa i p


> Ejercicio 4:

-- 4.A

cantidadDeAceitunas Prepizza = cantidadDeAceitunas (conDescripcionMejorada Prepizza)

-- LADO IZQUIERDO

    cantAcs Prepizza
=                                       (cantAcs.1)
    0

-- LADO DERECHO

    cantAcs (conDescM Prepizza)
=                                       (conDescM.1)
    cantAcs Prepizza
=                                       (cantAcs.1)
    0

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.B

cantidadDeAceitunas (Capa Queso Prepizza) = cantidadDeAceitunas (conDescripcionMejorada (Capa Queso Prepizza))

-- LADO IZQUIERDO

    cantAcs (Capa Queso Prepizza)
=                                                   (cantAcs.2)
    cantAceitunas Queso + cantAcs Prepizza
=                                                   (cantAceitunas.2)
    0 + cantAcs Prepizza
=                                                   (cantAcs.1)
    0 + 0
=                                                   (aritmética)
    0

-- LADO DERECHO

    cantAcs (conDescM (Capa Queso Prepizza))
=                                                   (conDescM.2)
    cantAcs (juntarAc Queso (conDescM Prepizza))
=                                                   (conDescM.1)
    cantAcs (juntarAc Queso Prepizza)
=                                                   (juntarAc.2)
    cantAcs Prepizza
=                                                   (cantAcs.1)
    0

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.C

cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza)) = 
cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza)))

-- LADO IZQUIERDO

    cantAcs (C (Ac 8) (C Q Pp))
=                                                           (cantAcs.2)
    cantAceitunas (Ac 8) + cantAcs (C Q Pp)
=                                                           (cantAcs.2)
    cantAceitunas (Ac 8) + cantAceitunas Q + cantAcs Pp
=                                                           (cantAcs.1)
    cantAceitunas (Ac 8) + cantAceitunas Q + 0
=                                                           (cantAceitunas.1)
    8 + cantAceitunas Q + 0
=                                                           (cantAceitunas.2)
    8 + 0 + 0
=                                                           (aritmética)
    8

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
=                                                           (cantAcs.2)
    cantAceitunas (Ac 8) + cantAcs (C Q Pp)
=                                                           (cantAceitunas.1)
    8 + cantAcs (C Q Pp)
=                                                           (cantAcs.1)
    8 + cantAceitunas Q + + cantAcs Pp
=                                                           (cantAceitunas.2)
    8 + 0 + cantAcs Pp
=                                                           (cantAcs.1)
    8 + 0 + 0
=                                                           (aritmética)
    8

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.D

cantidadDeAceitunas (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))) = 
cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))))

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


--------------------------------------------------------------------------------------------------------

## SECCIÓN 2

type Nombre = String 

data Planilla = Fin | Registro Nombre Planilla 

data Equipo = Becario Nombre 
            | Investigador Nombre Equipo Equipo Equipo


> Ejercicio 1:

PLANILLA:
    -- Aseguramos que el caso base exista en el conjunto.
    Regla base: Fin existe en el conjunto Planilla. 
    -- Aseguramos que el caso inductivo exista en el conjunto. 
    Regla inductiva: Sea que n pertenece al conjunto Nombre y p pertenece al conjunto Planilla, entonces
                     Registro n p pertenece al conjunto Planilla. 

EQUIPO:
    -- Aseguramos que el caso base exista en el conjunto.
    Regla base: Becario n existe en el conjunto Equipo, además que n pertenece al conjunto Nombre.
    -- Aseguramos que el caso inductivo exista en el conjunto. 
    Regla inductiva: Sea que n pertenece al conjunto Nombre, e1 pertenece al conjunto Equipo, e2 pertenece
                     al conjunto equipo, e3 pertenece al conjunto equipo, entonces Ingestigador n e1 e2 e3 
                     pertenece al conjunto Equipo.


> Ejercicio 2:

f :: Plantilla -> ...
f Fin            = ...
f (Registro n p) = ... n ... f p ... 

f :: Equipo -> ...
f (Becario n)               = ... n ...
f (Ingestigador n e1 e2 e3) = ... n ... f e1 ... f e2 ... f e3 ...


> Ejercicio 3:

-- 3.A

largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin            = 0
largoDePlanilla (Registro n p) = 1 + largoDePlanilla p 


-- 3.B

esta :: Nombre -> Planilla -> Bool
esta n' Fin            = False
esta n' (Registro n p) = n' == n || esta n' p 


-- 3.C

juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin            p' = p'
juntarPlanillas (Registro n p) p' = Registro n (juntarPlanillas p p') 


-- 3.D

nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario n)               = 0
nivelesJerarquicos (Ingestigador n e1 e2 e3) = 1 + max 
                                                  (max (nivelesJerarquicos e1) (nivelesJerarquicos e2))  
                                                  nivelesJerarquicos e3


-- 3.E

cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario n)               = 1
cantidadDeIntegrantes (Ingestigador n e1 e2 e3) = 1 + cantidadDeIntegrantes e1 
                                                    + cantidadDeIntegrantes e2 
                                                    + cantidadDeIntegrantes e3


-- 3.F

planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n)               = Registro n Fin
planillaDeIntegrantes (Ingestigador n e1 e2 e3) = Registro n (juntarPlanillas 
                                                                (juntarPlanillas (planillaDeIntegrantes e1)
                                                                                 (planillaDeIntegrantes e2)) 
                                                              (planillaDeIntegrantes e3))


> Ejercicio 4:

-- 4.A

Para todo p :: Planilla . largoDePlanilla (juntarPlanillas Fin p) = largoDePlanilla Fin + largoDePlanilla p

-- LADO IZQUIERDO

    largoDePlanilla (juntarPlanillas Fin p)
=                                               (juntarPlanillas.1) 
    largoDePlanilla p

-- LADO DERECHO

    largoDePlanilla Fin + largoDePlanilla p
=                                               (largoDePlanilla.1)
    0 + largoDePlanilla p
=                                               (aritmética)
    largoDePlanilla p

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.B

Para todo p :: Planilla . 
                largoDePlanilla  (juntarPlanillas (Registro "Edsger" Fin) p) = 
                largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p

-- LADO IZQUIERDO

    largoDePlanilla  (juntarPlanillas (Registro "Edsger" Fin) p)
=                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Edsger" (juntarPlanillas Fin p))
=                                                                   (juntarPlanillas.1)
    largoDePlanilla (Registro "Edsger" p)
=                                                                   (largoDePlanilla.2)
    1 + largoDePlanilla p

-- LADO DERECHO

    largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p
=                                                                   (largoDePlanilla.2)
    1 + largoDePlanilla Fin + largoDePlanilla p
=                                                                   (largoDePlanilla.1)
    1 + 0 + largoDePlanilla p
=                                                                   (aritmética)
    1 + largoDePlanilla p 

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.C

Para todo p :: Planilla . largoDePlanilla (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin)) p) = 
                          largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla p

-- LADO IZQUIERDO 

    largoDePlanilla (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin)) p)
=                                                                                   (juntarPlanillas.2)  
    largoDePlanilla (Registro "Alan" (juntarPlanillas (Registro "Edsger" Fin) p))
=                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Alan" (Registro "Edsger" (juntarPlanillas Fin p)))
=                                                                                   (juntarPlanillas.1)
    largoDePlanilla (Registro "Alan" (Registro "Edsger" p))
=                                                                                   (largoDePlanilla.2)
    1 + largoDePlanilla (Registro "Edsger" p)
=                                                                                   (largoDePlanilla.2)
    1 + 1 + largoDePlanilla p
=                                                                                   (aritmética)
    2 + largoDePlanilla p

-- LADO DERECHO

    largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla p
=                                                                                   (largoDePlanilla.2)
    1 + largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p
=                                                                                   (largoDePlanilla.2)
    1 + 1 + largoDePlanilla Fin + largoDePlanilla p
=                                                                                   (largoDePlanilla.1)
    1 + 1 + 0 + largoDePlanilla p
=                                                                                   (aritmética)
    2 + largoDePlanilla p

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.D

Para todo p :: Planilla . largoDePlanilla (juntarPlanillas (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) p) = 
                          largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) + largoDePlanilla p

-- LADO IZQUIERDO

    largoDePlanilla (juntarPlanillas (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) p)
=                                                                                                       (juntarPlanillas.2)
    largoDePlanilla (Registro "Alonzo" (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin)) p))
=                                                                                                       (juntarPlanillas.2)
    largoDePlanilla (Registro "Alonzo" (Registro "Alan" (juntarPlanillas (Registro "Edsger" Fin) p)))
=                                                                                                       (juntarPlanillas.2)
    largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" (juntarPlanillas Fin p))))
=                                                                                                       (juntarPlanillas.1)
    largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" p)))
=                                                                                                       (largoDePlanilla.2)
    1 + largoDePlanilla (Registro "Alan" (Registro "Edsger" p))
=                                                                                                       (largoDePlanilla.2)
    1 + 1 + largoDePlanilla (Registro "Edsger" p)
=                                                                                                       (largoDePlanilla.2)
    1 + 1 + 1 + largoDePlanilla p
=                                                                                                       (aritmética)
    3 + largoDePlanilla p

-- LADO DERECHO

    largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) + largoDePlanilla p
=                                                                                                       (largoDePlanilla.2)
    1 + largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla p
=                                                                                                       (largoDePlanilla.2)
    1 + 1 + largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p
=                                                                                                       (largoDePlanilla.2)
    1 + 1 + 1 + largoDePlanilla Fin + largoDePlanilla p
=                                                                                                       (largoDePlanilla.1)
    1 + 1 + 1 + 0 + largoDePlanilla p
=                                                                                                       (aritmética)
    3 + largoDePlanilla p

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


> Ejercicio 5

-- 5.A

largoDePlanilla (planillaDeIntegrantes (Becario "Alan")) = cantidadDeIntegrantes (Becario "Alan")

-- LADO IZQUIERDO

    largoDePlanilla (planillaDeIntegrantes (Becario "Alan")) 
=                                                               (planillaDeIntegrantes.1)
    largoDePlanilla (Registro "Alan" Fin)
=                                                               (largoDePlanilla.2)
    1 + largoDePlanilla Fin
=                                                               (largoDePlanilla.1)
    1 + 0
=                                                               (aritmética)
    1

-- LADO DERECHO

    cantidadDeIntegrantes (Becario "Alan")
=                                                               (cantidadDeIntegrantes.1)
    1

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 5.B

largoDePlanilla (planillaDeIntegrantes (Becario "Brian")) = cantidadDeIntegrantes (Becario "Brian")

-- LADO IZQUIERDO

    largoDePlanilla (planillaDeIntegrantes (Becario "Brian")) 
=                                                               (planillaDeIntegrantes.1)
    largoDePlanilla (Registro "Brian" Fin)
=                                                               (largoDePlanilla.2)
    1 + largoDePlanilla Fin
=                                                               (largoDePlanilla.1)
    1 + 0
=                                                               (aritmética)
    1

-- LADO DERECHO

    cantidadDeIntegrantes (Becario "Brian")
=                                                               (cantidadDeIntegrantes.1)
    1

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 5.C

Para todo n :: Nombre . largoDePlanilla (planillaDeIntegrantes (Becario n)) = cantidadDeIntegrantes (Becario n)

-- LADO IZQUIERDO

    largoDePlanilla (planillaDeIntegrantes (Becario n))
=                                                               (planillaDeIntegrantes.1)
    largoDePlanilla (Registro n Fin)
=                                                               (largoDePlanilla.2)
    1 + largoDePlanilla Fin
=                                                               (largoDePlanilla.1)
    1 + 0
=                                                               (aritmética)
    1

-- LADO DERECHO

    cantidadDeIntegrantes (Becario n)
=                                                               (cantidadDeIntegrantes.1)
    1

    -- Ambos lados llegan a lo mismo, la propiedad es válida.                                                         (largoDePlanilla.2)


-- 5.D

largoDePlanilla (planillaDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))) = 
cantidadDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))

-- LADO IZQUIERDO

    largoDePlanilla (planillaDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen")))
=                                                                                                   (planillaDeIntegrantes.2)
    largoDePlanilla (Registro "Alonzo" (juntarPlanillas
                                            (juntarPlanillas 
                                                            (planillaDeIntegrantes (Becario "Alan"))
                                                            (planillaDeIntegrantes (Becario "Alfred"))
                                            ) 
                                            (planillaDeIntegrantes (Becario "Stephen"))
                                        )
                    )
=                                                                                                   (planillaDeIntegrantes.1)
    largoDePlanilla (Registro "Alonzo" (juntarPlanillas
                                            (juntarPlanillas 
                                                            (Registro "Alan" Fin)
                                                            (planillaDeIntegrantes (Becario "Alfred"))
                                            ) 
                                            (planillaDeIntegrantes (Becario "Stephen"))
                                        )
                    )
=                                                                                                   (planillaDeIntegrantes.1)
    largoDePlanilla (Registro "Alonzo" (juntarPlanillas
                                            (juntarPlanillas 
                                                            (Registro "Alan" Fin)
                                                            (Registro "Alfred" Fin)
                                            ) 
                                            (planillaDeIntegrantes (Becario "Stephen"))
                                        )
                    )
=                                                                                                   (planillaDeIntegrantes.1)
    largoDePlanilla (Registro "Alonzo" (juntarPlanillas
                                            (juntarPlanillas 
                                                            (Registro "Alan" Fin)
                                                            (Registro "Alfred" Fin)
                                            ) 
                                            (Registro "Stephen" Fin)
                                        )
                    )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Alonzo" (juntarPlanillas
                                            (Registro "Alan"
                                                        (juntarPlanillas 
                                                                Fin 
                                                                (Registro "Alfred" Fin)
                                                        )
                                            ) 
                                            (Registro "Stephen" Fin)
                                        )
                    )
=                                                                                                   (juntarPlanillas.1)
    largoDePlanilla (Registro "Alonzo" (juntarPlanillas
                                            (Registro "Alan" (Registro "Alfred" Fin)) 
                                            (Registro "Stephen" Fin)
                                        )
                    )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Alonzo" (Registro "Alan" 
                                            (juntarPlanillas
                                                (Registro "Alfred" Fin)
                                                (Registro "Stephen" Fin)
                                            )
                                        )
                    )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Alonzo" (Registro "Alan" 
                                            (Registro "Alfred" 
                                                (juntarPlanillas
                                                    Fin
                                                    (Registro "Stephen" Fin)
                                                )
                                            )
                                        )
                    )
=                                                                                                   (juntarPlanillas.1)
    largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Alfred" (Registro "Stephen" Fin))))
=                                                                                                   (largoDePlanilla.2)
    1 + largoDePlanilla (Registro "Alan" (Registro "Alfred" (Registro "Stephen" Fin)))
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + largoDePlanilla (Registro "Alfred" (Registro "Stephen" Fin))
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + 1 + largoDePlanilla (Registro "Stephen" Fin)
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + 1 + 1 + largoDePlanilla Fin
=                                                                                                   (largoDePlanilla.1)
    1 + 1 + 1 + 1 + 0
=                                                                                                   (aritmética)
    4

-- LADO DERECHO

    cantidadDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))
=                                                                                                   (cantidadDeIntegrantes.2)
    1 + cantidadDeIntegrantes (Becario "Alan") + cantidadDeIntegrantes (Becario "Alfred") + cantidadDeIntegrantes (Becario "Stephen")
=                                                                                                   (cantidadDeIntegrantes.1)
    1 + 1 + cantidadDeIntegrantes (Becario "Alfred") + cantidadDeIntegrantes (Becario "Stephen")
=                                                                                                   (cantidadDeIntegrantes.1)
    1 + 1 + 1 + cantidadDeIntegrantes (Becario "Stephen")
=                                                                                                   (cantidadDeIntegrantes.1)
    1 + 1 + 1 + 1
=                                                                                                   (aritmética)
    4

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 5.E

para todo n :: Nombre . para todo n1 :: Nombre . para todo n2 :: Nombre . para todo n3 :: Nombre . 
largoDePlanilla (planillaDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3))) =
cantidadDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3))

-- LADO IZQUIERDO

    largoDePlanilla (planillaDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3)))
=                                                                                                   (planillaDeIntegrantes.2)
    largoDePlanilla (Registro n (juntarPlanillas 
                                            (juntarPlanillas 
                                                    (planillaDeIntegrantes (Becario n1))
                                                    (planillaDeIntegrantes (Becario n2))
                                            ) 
                                            (planillaDeIntegrantes (Becario n3))
                                )
                    )
=                                                                                                   (planillaDeIntegrantes.1)
    largoDePlanilla (Registro n (juntarPlanillas 
                                            (juntarPlanillas 
                                                    (Registro n1 Fin)
                                                    (planillaDeIntegrantes (Becario n2))
                                            ) 
                                            (planillaDeIntegrantes (Becario n3))
                                )
                    )
=                                                                                                   (planillaDeIntegrantes.1)
    largoDePlanilla (Registro n (juntarPlanillas 
                                            (juntarPlanillas 
                                                    (Registro n1 Fin)
                                                    (Registro n2 Fin)
                                            ) 
                                            (planillaDeIntegrantes (Becario n3))
                                )
                    )
=                                                                                                   (planillaDeIntegrantes.1)
    largoDePlanilla (Registro n (juntarPlanillas 
                                            (juntarPlanillas 
                                                    (Registro n1 Fin)
                                                    (Registro n2 Fin)
                                            ) 
                                            (Registro n3 Fin)
                                )
                    )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro n (juntarPlanillas 
                                            (Registro n1 
                                                    (juntarPlanillas
                                                        Fin
                                                        (Registro n2 Fin)
                                                    ) 
                                            ) 
                                            (Registro n3 Fin)
                                )
                    )
=                                                                                                   (juntarPlanillas.1)
    largoDePlanilla (Registro n (juntarPlanillas 
                                            (Registro n1 (Registro n2 Fin)) 
                                            (Registro n3 Fin)
                                )
                    )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro n (Registro n1
                                    (juntarPlanillas
                                        (Registro n2 Fin)
                                        (Registro n3 Fin)
                                    )
                                )
                    )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro n (Registro n1
                                    (Registrar n2
                                        (juntarPlanillas
                                            Fin
                                            (Registro n3 Fin)
                                        )
                                    )
                                )
                    )
=                                                                                                   (juntarPlanillas.1)
    largoDePlanilla (Registro n (Registro n1 (Registro n2 (Registro n3 Fin))))
=                                                                                                   (largoDePlanilla.2)
    1 + largoDePlanilla (Registro n1 (Registro n2 (Registro n3 Fin)))
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + largoDePlanilla (Registro n2 (Registro n3 Fin))
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + 1 + largoDePlanilla (Registro n3 Fin)
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + 1 + 1 + largoDePlanilla Fin
=                                                                                                   (largoDePlanilla.1)
    1 + 1 + 1 + 1 + 0
=                                                                                                   (aritmética)
    4

    cantidadDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3))
=                                                                                                   (cantidadDeIntegrantes.2)
    1 + cantidadDeIntegrantes (Becario n1) + cantidadDeIntegrantes (Becario n2) + cantidadDeIntegrantes (Becario n3)
=                                                                                                   (cantidadDeIntegrantes.1)
    1 + 1 + cantidadDeIntegrantes (Becario n2) + cantidadDeIntegrantes (Becario n3)
=                                                                                                   (cantidadDeIntegrantes.1)
    1 + 1 + 1 + cantidadDeIntegrantes (Becario n3)
=                                                                                                   (cantidadDeIntegrantes.1)
    1 + 1 + 1 + 1
=                                                                                                   (aritmética)
    4

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 5.F

largoDePlanilla (planillaDeIntegrantes 
                    (Investigador "Oswald" 
                       (Investigador "Alonzo" 
                                (Becario "Alan") 
                                (Becario "Alfred") 
                                (Becario "Stephen")
                        ) 
                        (Investigador "John"   
                                (Becario "Brian") 
                                (Becario "Graham") 
                                (Becario "Ioan")
                        ) 
                        (Investigador "Robert" 
                                (Becario "Gordon") 
                                (Becario "John") 
                                (Becario "Raymond")
                        )
                    )
                ) = 
cantidadDeIntegrantes (Investigador "Oswald" 
                            (Investigador "Alonzo" 
                                    (Becario "Alan") 
                                    (Becario "Alfred") 
                                    (Becario "Stephen")
                            ) 
                            (Investigador "John"   
                                    (Becario "Brian") 
                                    (Becario "Graham") 
                                    (Becario "Ioan")
                            ) 
                            (Investigador "Robert" 
                                    (Becario "Gordon") 
                                    (Becario "John") 
                                    (Becario "Raymond")
                            )
                        )

-- LADO IZQUIERDO

    largoDePlanilla (planillaDeIntegrantes 
                        (Investigador "Oswald" 
                        (Investigador "Alonzo" 
                                    (Becario "Alan") 
                                    (Becario "Alfred") 
                                    (Becario "Stephen")
                            ) 
                            (Investigador "John"   
                                    (Becario "Brian") 
                                    (Becario "Graham") 
                                    (Becario "Ioan")
                            ) 
                            (Investigador "Robert" 
                                    (Becario "Gordon") 
                                    (Becario "John") 
                                    (Becario "Raymond")
                            )
                        )
                    )
=                                                                                                   (planillaDeIntegrantes.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (planillaDeIntegrantes 
                                                (Investigador "Alonzo" 
                                                    (Becario "Alan") 
                                                    (Becario "Alfred") 
                                                    (Becario "Stephen")
                                                ) 
                                            )
                                            (planillaDeIntegrantes
                                                (Investigador "John"   
                                                    (Becario "Brian") 
                                                    (Becario "Graham") 
                                                    (Becario "Ioan")
                                                ) 
                                            )
                                    ) 
                                    (planillaDeIntegrantes
                                        (Investigador "Robert" 
                                            (Becario "Gordon") 
                                            (Becario "John") 
                                            (Becario "Raymond")
                                        )
                                    )
                            )
                    )
=                                                                                                   (planillaDeIntegrantes.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (planillaDeIntegrantes 
                                                (Investigador "Alonzo" 
                                                    (Becario "Alan") 
                                                    (Becario "Alfred") 
                                                    (Becario "Stephen")
                                                ) 
                                            )
                                            (planillaDeIntegrantes
                                                (Investigador "John"   
                                                    (Becario "Brian") 
                                                    (Becario "Graham") 
                                                    (Becario "Ioan")
                                                ) 
                                            )
                                    ) 
                                    (Registro "Robert"
                                            (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (planillaDeIntegrantes (Becario "Gordon") )
                                                        (planillaDeIntegrantes (Becario "John"))
                                                    ) 
                                                    (planillaDeIntegrantes (Becario "Raymond"))
                                            )
                                    )
                            )
                    )
=                                                                                                   (planillaDeIntegrantes.1)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (planillaDeIntegrantes 
                                                (Investigador "Alonzo" 
                                                    (Becario "Alan") 
                                                    (Becario "Alfred") 
                                                    (Becario "Stephen")
                                                ) 
                                            )
                                            (planillaDeIntegrantes
                                                (Investigador "John"   
                                                    (Becario "Brian") 
                                                    (Becario "Graham") 
                                                    (Becario "Ioan")
                                                ) 
                                            )
                                    ) 
                                    (Registro "Robert"
                                            (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (Registro "Gordon" Fin)
                                                        (planillaDeIntegrantes (Becario "John"))
                                                    ) 
                                                    (planillaDeIntegrantes (Becario "Raymond"))
                                            )
                                    )
                            )
                    )
=                                                                                                   (planillaDeIntegrantes.1)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (planillaDeIntegrantes 
                                                (Investigador "Alonzo" 
                                                    (Becario "Alan") 
                                                    (Becario "Alfred") 
                                                    (Becario "Stephen")
                                                ) 
                                            )
                                            (planillaDeIntegrantes
                                                (Investigador "John"   
                                                    (Becario "Brian") 
                                                    (Becario "Graham") 
                                                    (Becario "Ioan")
                                                ) 
                                            )
                                    ) 
                                    (Registro "Robert"
                                            (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (Registro "Gordon" Fin)
                                                        (Registro "John" Fin)
                                                    ) 
                                                    (planillaDeIntegrantes (Becario "Raymond"))
                                            )
                                    )
                            )
                    )
=                                                                                                   (planillaDeIntegrantes.1)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (planillaDeIntegrantes 
                                                (Investigador "Alonzo" 
                                                    (Becario "Alan") 
                                                    (Becario "Alfred") 
                                                    (Becario "Stephen")
                                                ) 
                                            )
                                            (planillaDeIntegrantes
                                                (Investigador "John"   
                                                    (Becario "Brian") 
                                                    (Becario "Graham") 
                                                    (Becario "Ioan")
                                                ) 
                                            )
                                    ) 
                                    (Registro "Robert"
                                            (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (Registro "Gordon" Fin)
                                                        (Registro "John" Fin)
                                                    ) 
                                                    (Registro "Raymond" Fin)
                                            )
                                    )
                            )
                    )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (planillaDeIntegrantes 
                                                (Investigador "Alonzo" 
                                                    (Becario "Alan") 
                                                    (Becario "Alfred") 
                                                    (Becario "Stephen")
                                                ) 
                                            )
                                            (planillaDeIntegrantes
                                                (Investigador "John"   
                                                    (Becario "Brian") 
                                                    (Becario "Graham") 
                                                    (Becario "Ioan")
                                                ) 
                                            )
                                    ) 
                                    (Registro "Robert"
                                            (juntarPlanillas 
                                                    (Registro "Gordon"
                                                        (juntarPlanillas
                                                            Fin
                                                            (Registro "John" Fin)
                                                        )
                                                    ) 
                                                    (Registro "Raymond" Fin)
                                            )
                                    )
                            )
                    )
=                                                                                                   (juntarPlanillas.1)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (planillaDeIntegrantes 
                                                (Investigador "Alonzo" 
                                                    (Becario "Alan") 
                                                    (Becario "Alfred") 
                                                    (Becario "Stephen")
                                                ) 
                                            )
                                            (planillaDeIntegrantes
                                                (Investigador "John"   
                                                    (Becario "Brian") 
                                                    (Becario "Graham") 
                                                    (Becario "Ioan")
                                                ) 
                                            )
                                    ) 
                                    (Registro "Robert"
                                            (juntarPlanillas 
                                                    (Registro "Gordon" (Registro "John" Fin)) 
                                                    (Registro "Raymond" Fin)
                                            )
                                    )
                            )   
                    )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (planillaDeIntegrantes 
                                                (Investigador "Alonzo" 
                                                    (Becario "Alan") 
                                                    (Becario "Alfred") 
                                                    (Becario "Stephen")
                                                ) 
                                            )
                                            (planillaDeIntegrantes
                                                (Investigador "John"   
                                                    (Becario "Brian") 
                                                    (Becario "Graham") 
                                                    (Becario "Ioan")
                                                ) 
                                            )
                                    ) 
                                    (Registro "Robert"
                                            (Registro "Gordon"
                                                (juntarPlanillas
                                                    (Registro "John" Fin)
                                                    (Registro "Raymond" Fin)
                                                )
                                            )
                                    )
                            )
                    )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (planillaDeIntegrantes 
                                                (Investigador "Alonzo" 
                                                    (Becario "Alan") 
                                                    (Becario "Alfred") 
                                                    (Becario "Stephen")
                                                ) 
                                            )
                                            (planillaDeIntegrantes
                                                (Investigador "John"   
                                                    (Becario "Brian") 
                                                    (Becario "Graham") 
                                                    (Becario "Ioan")
                                                ) 
                                            )
                                    ) 
                                    (Registro "Robert"
                                            (Registro "Gordon"
                                                (Registro "John" 
                                                    (juntarPlanillas
                                                        Fin
                                                        (Registro "Raymond" Fin)
                                                    )
                                                )
                                            )
                                    )
                            )
                        )
=                                                                                                   (juntarPlanillas.1)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (planillaDeIntegrantes 
                                                (Investigador "Alonzo" 
                                                    (Becario "Alan") 
                                                    (Becario "Alfred") 
                                                    (Becario "Stephen")
                                                ) 
                                            )
                                            (planillaDeIntegrantes
                                                (Investigador "John"   
                                                    (Becario "Brian") 
                                                    (Becario "Graham") 
                                                    (Becario "Ioan")
                                                ) 
                                            )
                                    ) 
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            )
                        )
=                                                                                                   (planillaDeIntegrantes.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (planillaDeIntegrantes 
                                                (Investigador "Alonzo" 
                                                    (Becario "Alan") 
                                                    (Becario "Alfred") 
                                                    (Becario "Stephen")
                                                ) 
                                            )
                                            (Registro "John"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (planillaDeIntegrantes (Becario "Brian"))
                                                        (planillaDeIntegrantes (Becario "Graham"))
                                                    ) 
                                                    (planillaDeIntegrantes (Becario "Ioan"))
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (planillaDeIntegrantes.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (planillaDeIntegrantes (Becario "Alan"))
                                                        (planillaDeIntegrantes (Becario "Alfred"))
                                                    ) 
                                                    (planillaDeIntegrantes (Becario "Stephen"))
                                                )
                                            ) 
                                            (Registro "John"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (planillaDeIntegrantes (Becario "Brian"))
                                                        (planillaDeIntegrantes (Becario "Graham"))
                                                    ) 
                                                    (planillaDeIntegrantes (Becario "Ioan"))
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (planillaDeIntegrantes.1)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (planillaDeIntegrantes (Becario "Alan"))
                                                        (planillaDeIntegrantes (Becario "Alfred"))
                                                    ) 
                                                    (planillaDeIntegrantes (Becario "Stephen"))
                                                )
                                            ) 
                                            (Registro "John"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (Registro "Brian" Fin)
                                                        (planillaDeIntegrantes (Becario "Graham"))
                                                    ) 
                                                    (planillaDeIntegrantes (Becario "Ioan"))
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (planillaDeIntegrantes.1)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (planillaDeIntegrantes (Becario "Alan"))
                                                        (planillaDeIntegrantes (Becario "Alfred"))
                                                    ) 
                                                    (planillaDeIntegrantes (Becario "Stephen"))
                                                )
                                            ) 
                                            (Registro "John"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (Registro "Brian" Fin)
                                                        (Registro "Graham" Fin)
                                                    ) 
                                                    (planillaDeIntegrantes (Becario "Ioan"))
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (planillaDeIntegrantes.1)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (planillaDeIntegrantes (Becario "Alan"))
                                                        (planillaDeIntegrantes (Becario "Alfred"))
                                                    ) 
                                                    (planillaDeIntegrantes (Becario "Stephen"))
                                                )
                                            ) 
                                            (Registro "John"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (Registro "Brian" Fin)
                                                        (Registro "Graham" Fin)
                                                    ) 
                                                    (Registro "Ioan" Fin)
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (planillaDeIntegrantes.1)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (Registro "Alan" Fin)
                                                        (planillaDeIntegrantes (Becario "Alfred"))
                                                    ) 
                                                    (planillaDeIntegrantes (Becario "Stephen"))
                                                )
                                            ) 
                                            (Registro "John"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (Registro "Brian" Fin)
                                                        (Registro "Graham" Fin)
                                                    ) 
                                                    (Registro "Ioan" Fin)
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (planillaDeIntegrantes.1)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (Registro "Alan" Fin)
                                                        (Registro "Alfred" Fin)
                                                    ) 
                                                    (planillaDeIntegrantes (Becario "Stephen"))
                                                )
                                            ) 
                                            (Registro "John"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (Registro "Brian" Fin)
                                                        (Registro "Graham" Fin)
                                                    ) 
                                                    (Registro "Ioan" Fin)
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (planillaDeIntegrantes.1)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (Registro "Alan" Fin)
                                                        (Registro "Alfred" Fin)
                                                    ) 
                                                    (Registro "Stephen" Fin)
                                                )
                                            ) 
                                            (Registro "John"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (Registro "Brian" Fin)
                                                        (Registro "Graham" Fin)
                                                    ) 
                                                    (Registro "Ioan" Fin)
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (Registro "Alan" Fin)
                                                        (Registro "Alfred" Fin)
                                                    ) 
                                                    (Registro "Stephen" Fin)
                                                )
                                            ) 
                                            (Registro "John"
                                                (juntarPlanillas 
                                                    (Registro "Brian"
                                                        (juntarPlanillas
                                                            Fin
                                                            (Registro "Graham" Fin)
                                                        ) 
                                                    ) 
                                                    (Registro "Ioan" Fin)
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (juntarPlanillas.1)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo"
                                                (juntarPlanillas 
                                                    (juntarPlanillas 
                                                        (Registro "Alan" Fin)
                                                        (Registro "Alfred" Fin)
                                                    ) 
                                                    (Registro "Stephen" Fin)
                                                )
                                            ) 
                                            (Registro "John"
                                                (juntarPlanillas 
                                                    (Registro "Brian" (Registro "Graham" Fin)) 
                                                    (Registro "Ioan" Fin)
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo"
                                                (juntarPlanillas 
                                                    (Registro "Alan"
                                                        (juntarPlanillas
                                                            Fin
                                                            (Registro "Alfred" Fin)
                                                        ) 
                                                    ) 
                                                    (Registro "Stephen" Fin)
                                                )
                                            ) 
                                            (Registro "John"
                                                (juntarPlanillas 
                                                    (Registro "Brian" (Registro "Graham" Fin)) 
                                                    (Registro "Ioan" Fin)
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (juntarPlanillas.1)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo"
                                                (juntarPlanillas 
                                                    (Registro "Alan" (Registro "Alfred" Fin)) 
                                                    (Registro "Stephen" Fin)
                                                )
                                            ) 
                                            (Registro "John"
                                                (juntarPlanillas 
                                                    (Registro "Brian" (Registro "Graham" Fin)) 
                                                    (Registro "Ioan" Fin)
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo"
                                                (juntarPlanillas 
                                                    (Registro "Alan" (Registro "Alfred" Fin)) 
                                                    (Registro "Stephen" Fin)
                                                )
                                            ) 
                                            (Registro "John"
                                                (Registro "Brian"
                                                    (juntarPlanillas
                                                        (Registro "Graham" Fin)
                                                        (Registro "Ioan" Fin)
                                                    )
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo"
                                                (Registro "Alan"
                                                    (juntarPlanillas
                                                        (Registro "Alfred" Fin)
                                                        (Registro "Stephen" Fin)
                                                    )
                                                )
                                            ) 
                                            (Registro "John"
                                                (Registro "Brian"
                                                    (juntarPlanillas
                                                        (Registro "Graham" Fin)
                                                        (Registro "Ioan" Fin)
                                                    )
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo"
                                                (Registro "Alan"
                                                    (juntarPlanillas
                                                        (Registro "Alfred" Fin)
                                                        (Registro "Stephen" Fin)
                                                    )
                                                )
                                            ) 
                                            (Registro "John"
                                                (Registro "Brian"
                                                    (Registro "Graham"
                                                        (juntarPlanillas
                                                            Fin
                                                            (Registro "Ioan" Fin)
                                                        )
                                                    )
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo"
                                                (Registro "Alan"
                                                    (Registro "Alfred"
                                                        (juntarPlanillas
                                                            Fin
                                                            (Registro "Stephen" Fin)
                                                        )
                                                    )
                                                )
                                            ) 
                                            (Registro "John"
                                                (Registro "Brian"
                                                    (Registro "Graham"
                                                        (juntarPlanillas
                                                            Fin
                                                            (Registro "Ioan" Fin)
                                                        )
                                                    )
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (juntarPlanillas.1)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo" (Registro "Alan" (Registro "Alfred" (Registro "Stephen" Fin)))) 
                                            (Registro "John"
                                                (Registro "Brian"
                                                    (Registro "Graham"
                                                        (juntarPlanillas
                                                            Fin
                                                            (Registro "Ioan" Fin)
                                                        )
                                                    )
                                                )
                                            ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (juntarPlanillas.1)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (juntarPlanillas 
                                            (Registro "Alonzo" (Registro "Alan" (Registro "Alfred" (Registro "Stephen" Fin)))) 
                                            (Registro "John" (Registro "Brian" (Registro "Graham" (Registro "Ioan" Fin)))) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (Registro "Alonzo"
                                        (juntarPlanillas
                                            (Registro "Alan" (Registro "Alfred" (Registro "Stephen" Fin)))
                                            (Registro "John" (Registro "Brian" (Registro "Graham" (Registro "Ioan" Fin))))
                                        ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (Registro "Alonzo"
                                        (Registro "Alan" 
                                            (juntarPlanillas
                                                (Registro "Alfred" (Registro "Stephen" Fin))
                                                (Registro "John" (Registro "Brian" (Registro "Graham" (Registro "Ioan" Fin))))
                                            )
                                        ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (Registro "Alonzo"
                                        (Registro "Alan" 
                                            (Registro "Alfred"
                                                (juntarPlanillas
                                                    (Registro "Stephen" Fin)
                                                    (Registro "John" (Registro "Brian" (Registro "Graham" (Registro "Ioan" Fin))))
                                                )
                                            )
                                        ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (Registro "Alonzo"
                                        (Registro "Alan" 
                                            (Registro "Alfred"
                                                (Registro "Stephen"
                                                    (juntarPlanillas
                                                        Fin
                                                        (Registro "John" (Registro "Brian" (Registro "Graham" (Registro "Ioan" Fin))))
                                                    )
                                                )
                                            )
                                        ) 
                                    )
                                    (Registro "Robert" (Registro "Gordon" (Registro "John" (Registro "Raymond" Fin))))
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                    (Registro "Alonzo"
                                        (Registro "Alan" 
                                            (Registro "Alfred"
                                                (Registro "Stephen"
                                                        (Registro "John" (Registro "Brian" (Registro "Graham" (Registro "Ioan" Fin))))
                                                )
                                            )
                                        ) 
                                    )
                                    (Registro "Robert" 
                                        (Registro "Gordon" 
                                            (Registro "John"
                                                (Registro "Raymond" Fin)
                                            )
                                        )
                                    )
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (Registro "Alonzo"
                                (juntarPlanillas
                                    (Registro "Alan" 
                                        (Registro "Alfred"
                                            (Registro "Stephen"
                                                (Registro "John" (Registro "Brian" (Registro "Graham" (Registro "Ioan" Fin))))
                                            )
                                        )
                                    )
                                    (Registro "Robert" 
                                        (Registro "Gordon" 
                                            (Registro "John"
                                                (Registro "Raymond" Fin)
                                            )
                                        )
                                    )
                                )
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (Registro "Alonzo"
                                (Registro "Alan"
                                    (juntarPlanillas
                                        (Registro "Alfred"
                                            (Registro "Stephen"
                                                (Registro "John" 
                                                    (Registro "Brian" 
                                                        (Registro "Graham" 
                                                            (Registro "Ioan" Fin)
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                        (Registro "Robert" 
                                            (Registro "Gordon" 
                                                (Registro "John"
                                                    (Registro "Raymond" Fin)
                                                )
                                            )
                                        )
                                    )
                                )
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (Registro "Alonzo"
                                (Registro "Alan"
                                    (Registro "Alfred"
                                        (juntarPlanillas
                                            (Registro "Stephen"
                                                (Registro "John" 
                                                    (Registro "Brian" 
                                                        (Registro "Graham" 
                                                            (Registro "Ioan" Fin)
                                                        )
                                                    )
                                                )
                                            )
                                            (Registro "Robert" 
                                                (Registro "Gordon" 
                                                    (Registro "John"
                                                        (Registro "Raymond" Fin)
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (Registro "Alonzo"
                                (Registro "Alan"
                                    (Registro "Alfred"
                                        (juntarPlanillas
                                            (Registro "Stephen"
                                                (Registro "John" 
                                                    (Registro "Brian" 
                                                        (Registro "Graham" 
                                                            (Registro "Ioan" Fin)
                                                        )
                                                    )
                                                )
                                            )
                                            (Registro "Robert" 
                                                (Registro "Gordon" 
                                                    (Registro "John"
                                                        (Registro "Raymond" Fin)
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (Registro "Alonzo"
                                (Registro "Alan"
                                    (Registro "Alfred"
                                        (Registro "Stephen"
                                            (juntarPlanillas
                                                (Registro "John" 
                                                    (Registro "Brian" 
                                                        (Registro "Graham" 
                                                            (Registro "Ioan" Fin)
                                                        )
                                                    )
                                                )
                                                (Registro "Robert" 
                                                    (Registro "Gordon" 
                                                        (Registro "John"
                                                            (Registro "Raymond" Fin)
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (Registro "Alonzo"
                                (Registro "Alan"
                                    (Registro "Alfred"
                                        (Registro "Stephen"
                                            (Registro "John"
                                                (juntarPlanillas
                                                    (Registro "Brian" 
                                                        (Registro "Graham" 
                                                            (Registro "Ioan" Fin)
                                                        )
                                                    )
                                                    (Registro "Robert" 
                                                        (Registro "Gordon" 
                                                            (Registro "John"
                                                                (Registro "Raymond" Fin)
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (Registro "Alonzo"
                                (Registro "Alan"
                                    (Registro "Alfred"
                                        (Registro "Stephen"
                                            (Registro "John"
                                                (Registro "Brian"
                                                    (juntarPlanillas
                                                        (Registro "Graham" 
                                                            (Registro "Ioan" Fin)
                                                        )
                                                        (Registro "Robert" 
                                                            (Registro "Gordon" 
                                                                (Registro "John"
                                                                    (Registro "Raymond" Fin)
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (Registro "Alonzo"
                                (Registro "Alan"
                                    (Registro "Alfred"
                                        (Registro "Stephen"
                                            (Registro "John"
                                                (Registro "Brian"
                                                    (Registro "Graham" 
                                                        (juntarPlanillas
                                                            (Registro "Ioan" Fin)
                                                            (Registro "Robert" 
                                                                (Registro "Gordon" 
                                                                    (Registro "John"
                                                                        (Registro "Raymond" Fin)
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            ) 
                        )
=                                                                                                   (juntarPlanillas.2)
    largoDePlanilla (Registro "Oswald"
                            (Registro "Alonzo"
                                (Registro "Alan"
                                    (Registro "Alfred"
                                        (Registro "Stephen"
                                            (Registro "John"
                                                (Registro "Brian"
                                                    (Registro "Graham" 
                                                        (Registro "Ioan"
                                                            (juntarPlanillas
                                                                Fin
                                                                (Registro "Robert" 
                                                                    (Registro "Gordon" 
                                                                        (Registro "John"
                                                                            (Registro "Raymond" Fin)
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            ) 
                        )
=                                                                                                   (largoDePlanilla.2)
    largoDePlanilla (Registro "Oswald"
                        (Registro "Alonzo"
                            (Registro "Alan"
                                (Registro "Alfred"
                                    (Registro "Stephen"
                                        (Registro "John"
                                            (Registro "Brian"
                                                (Registro "Graham" 
                                                    (Registro "Ioan"
                                                        (Registro "Robert" 
                                                            (Registro "Gordon" 
                                                                (Registro "John"
                                                                    (Registro "Raymond" Fin)
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
=                                                                                                   (largoDePlanilla.2)
    1 + largoDePlanilla (Registro "Alonzo"
                            (Registro "Alan"
                                (Registro "Alfred"
                                    (Registro "Stephen"
                                        (Registro "John"
                                            (Registro "Brian"
                                                (Registro "Graham" 
                                                    (Registro "Ioan"
                                                        (Registro "Robert" 
                                                            (Registro "Gordon" 
                                                                (Registro "John"
                                                                    (Registro "Raymond" Fin)
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + largoDePlanilla (Registro "Alan"
                                (Registro "Alfred"
                                    (Registro "Stephen"
                                        (Registro "John"
                                            (Registro "Brian"
                                                (Registro "Graham" 
                                                    (Registro "Ioan"
                                                        (Registro "Robert" 
                                                            (Registro "Gordon" 
                                                                (Registro "John"
                                                                    (Registro "Raymond" Fin)
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + 1 + largoDePlanilla (Registro "Alfred"
                                    (Registro "Stephen"
                                        (Registro "John"
                                            (Registro "Brian"
                                                (Registro "Graham" 
                                                    (Registro "Ioan"
                                                        (Registro "Robert" 
                                                            (Registro "Gordon" 
                                                                (Registro "John"
                                                                    (Registro "Raymond" Fin)
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + 1 + 1 + largoDePlanilla (Registro "Stephen"
                                        (Registro "John"
                                            (Registro "Brian"
                                                (Registro "Graham" 
                                                    (Registro "Ioan"
                                                        (Registro "Robert" 
                                                            (Registro "Gordon" 
                                                                (Registro "John"
                                                                    (Registro "Raymond" Fin)
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + 1 + 1 + 1 + largoDePlanilla (Registro "John"
                                            (Registro "Brian"
                                                (Registro "Graham" 
                                                    (Registro "Ioan"
                                                        (Registro "Robert" 
                                                            (Registro "Gordon" 
                                                                (Registro "John"
                                                                    (Registro "Raymond" Fin)
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + 1 + 1 + 1 + 1 + largoDePlanilla (Registro "Brian"
                                                (Registro "Graham" 
                                                    (Registro "Ioan"
                                                        (Registro "Robert" 
                                                            (Registro "Gordon" 
                                                                (Registro "John"
                                                                    (Registro "Raymond" Fin)
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + largoDePlanilla (Registro "Ioan"
                                                        (Registro "Robert" 
                                                            (Registro "Gordon" 
                                                                (Registro "John"
                                                                    (Registro "Raymond" Fin)
                                                                )
                                                            )
                                                        )
                                                    )
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + largoDePlanilla (Registro "Robert" 
                                                            (Registro "Gordon" 
                                                                (Registro "John"
                                                                    (Registro "Raymond" Fin)
                                                                )
                                                            )
                                                        )
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + largoDePlanilla (Registro "Gordon" 
                                                                (Registro "John"
                                                                    (Registro "Raymond" Fin)
                                                                )
                                                            )
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + largoDePlanilla (Registro "John"
                                                                    (Registro "Raymond" Fin)
                                                                )
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + largoDePlanilla (Registro "Raymond" Fin)
=                                                                                                   (largoDePlanilla.2)
    1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + largoDePlanilla Fin
=                                                                                                   (largoDePlanilla.1)
    1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 0
=                                                                                                   (aritmética)
    13 

-- LADO DERECHO

    cantidadDeIntegrantes (Investigador "Oswald" 
                                (Investigador "Alonzo" 
                                        (Becario "Alan") 
                                        (Becario "Alfred") 
                                        (Becario "Stephen")
                                ) 
                                (Investigador "John"   
                                        (Becario "Brian") 
                                        (Becario "Graham") 
                                        (Becario "Ioan")
                                ) 
                                (Investigador "Robert" 
                                        (Becario "Gordon") 
                                        (Becario "John") 
                                        (Becario "Raymond")
                                )
                            )
=                                                                                                   (cantidadDeIntegrantes.2)
    1 + cantidadDeIntegrantes (Investigador "Alonzo" 
                                        (Becario "Alan") 
                                        (Becario "Alfred") 
                                        (Becario "Stephen")
                                )
      + cantidadDeIntegrantes (Investigador "John"   
                                        (Becario "Brian") 
                                        (Becario "Graham") 
                                        (Becario "Ioan")
                                )
      + cantidadDeIntegrantes (Investigador "Robert" 
                                        (Becario "Gordon") 
                                        (Becario "John") 
                                        (Becario "Raymond")
                                )
=                                                                                                   (cantidadDeIntegrantes.2)
    1 + 1 +  cantidadDeIntegrantes (Becario "Alan") + cantidadDeIntegrantes (Becario "Alfred") + cantidadDeIntegrantes (Becario "Stephen")
      + cantidadDeIntegrantes (Investigador "John"   
                                        (Becario "Brian") 
                                        (Becario "Graham") 
                                        (Becario "Ioan")
                                )
      + cantidadDeIntegrantes (Investigador "Robert" 
                                        (Becario "Gordon") 
                                        (Becario "John") 
                                        (Becario "Raymond")
                                )
=                                                                                                   (cantidadDeIntegrantes.2)
    1 + 1 +  cantidadDeIntegrantes (Becario "Alan") + cantidadDeIntegrantes (Becario "Alfred") + cantidadDeIntegrantes (Becario "Stephen")
      + cantidadDeIntegrantes (Becario "Brian") + cantidadDeIntegrantes (Becario "Graham") + cantidadDeIntegrantes (Becario "Ioan")
      + cantidadDeIntegrantes (Investigador "Robert" 
                                        (Becario "Gordon") 
                                        (Becario "John") 
                                        (Becario "Raymond")
                                )
=                                                                                                   (cantidadDeIntegrantes.2)
    1 + 1 + cantidadDeIntegrantes (Becario "Alan") + cantidadDeIntegrantes (Becario "Alfred") + cantidadDeIntegrantes (Becario "Stephen")
      + 1 + cantidadDeIntegrantes (Becario "Brian") + cantidadDeIntegrantes (Becario "Graham") + cantidadDeIntegrantes (Becario "Ioan")
      + 1 + cantidadDeIntegrantes (Becario "Gordon") + cantidadDeIntegrantes (Becario "John") + cantidadDeIntegrantes (Becario "Raymond")
=                                                                                                   (cantidadDeIntegrantes.1)
    1 + 1 + 1 + cantidadDeIntegrantes (Becario "Alfred") + cantidadDeIntegrantes (Becario "Stephen")
      + 1 + cantidadDeIntegrantes (Becario "Brian") + cantidadDeIntegrantes (Becario "Graham") + cantidadDeIntegrantes (Becario "Ioan")
      + 1 + cantidadDeIntegrantes (Becario "Gordon") + cantidadDeIntegrantes (Becario "John") + cantidadDeIntegrantes (Becario "Raymond")
=                                                                                                   (cantidadDeIntegrantes.1)
    1 + 1 + 1 + 1 + cantidadDeIntegrantes (Becario "Stephen")
      + 1 + cantidadDeIntegrantes (Becario "Brian") + cantidadDeIntegrantes (Becario "Graham") + cantidadDeIntegrantes (Becario "Ioan")
      + 1 + cantidadDeIntegrantes (Becario "Gordon") + cantidadDeIntegrantes (Becario "John") + cantidadDeIntegrantes (Becario "Raymond")
=                                                                                                   (cantidadDeIntegrantes.1)
    1 + 1 + 1 + 1 + 1
      + 1 + cantidadDeIntegrantes (Becario "Brian") + cantidadDeIntegrantes (Becario "Graham") + cantidadDeIntegrantes (Becario "Ioan")
      + 1 + cantidadDeIntegrantes (Becario "Gordon") + cantidadDeIntegrantes (Becario "John") + cantidadDeIntegrantes (Becario "Raymond")
=                                                                                                   (cantidadDeIntegrantes.1)
    1 + 1 + 1 + 1 + 1
      + 1 + 1 + cantidadDeIntegrantes (Becario "Graham") + cantidadDeIntegrantes (Becario "Ioan")
      + 1 + cantidadDeIntegrantes (Becario "Gordon") + cantidadDeIntegrantes (Becario "John") + cantidadDeIntegrantes (Becario "Raymond")
=                                                                                                   (cantidadDeIntegrantes.1)
    1 + 1 + 1 + 1 + 1
      + 1 + 1 + 1 + cantidadDeIntegrantes (Becario "Ioan")
      + 1 + cantidadDeIntegrantes (Becario "Gordon") + cantidadDeIntegrantes (Becario "John") + cantidadDeIntegrantes (Becario "Raymond")
=                                                                                                   (cantidadDeIntegrantes.1)
    1 + 1 + 1 + 1 + 1
      + 1 + 1 + 1 + 1
      + 1 + cantidadDeIntegrantes (Becario "Gordon") + cantidadDeIntegrantes (Becario "John") + cantidadDeIntegrantes (Becario "Raymond")
=                                                                                                   (cantidadDeIntegrantes.1)
    1 + 1 + 1 + 1 + 1
      + 1 + 1 + 1 + 1
      + 1 + 1 + cantidadDeIntegrantes (Becario "John") + cantidadDeIntegrantes (Becario "Raymond")
=                                                                                                   (cantidadDeIntegrantes.1)
    1 + 1 + 1 + 1 + 1
      + 1 + 1 + 1 + 1
      + 1 + 1 + 1 + cantidadDeIntegrantes (Becario "Raymond")
=                                                                                                   (cantidadDeIntegrantes.1)
    1 + 1 + 1 + 1 + 1
      + 1 + 1 + 1 + 1
      + 1 + 1 + 1 + 1
=                                                                                                   (aritmética)
    13

    -- Ambos lados llegan a lo mismo, la propiedad es válida.

--------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------

## SECCIÓN 3

data Dungeon a = Habitacion a 
               | Pasaje (Maybe a) (Dungeon a) 
               | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a)                   


> Ejercicio 1:

DUNGEON:
    -- Aseguramos que el caso base exista en el conjunto.
    Regla base: 
    -- Aseguramos que el caso inductivo exista en el conjunto. 
    Regla inductiva: 


> Ejercicio 2:

f :: Dungeon -> ...
f Fin            = ...
f (Registro n p) = ... n ... f p ... 
f (Ingestigador n e1 e2 e3) = ... n ... f e1 ... f e2 ... f e3 ...


> Ejercicio 3: