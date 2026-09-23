--------------------------------------------------------------------------------------------------------

--                      TRABAJO PRÁCTICO N°7: Inducción y Recursión Estructural I                     --

--------------------------------------------------------------------------------------------------------

## SECCIÓN 1:

data Pizza = Prepizza | Capa Ingrediente Pizza 
    deriving Show

data Ingrediente = Aceitunas Int | Anchoas | Cebolla 
                 | Jamón         | Queso   | Salsa
    deriving Show


> Ejercicio 1:

Reglas que definen el conjunto Pizza:
    > Regla base: Prepizza está en el conjunto Pizza.
    > Regla inductiva: Si i está en el conjunto Ingrediente, p está en el conjunto Pizza,
                       entonces Capa i p está en el conjunto Pizza.


> Ejercicio 2:

f :: Pizza -> a
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

#############################################################################################################################

## SECCIÓN 2:

type Nombre = String 

data Planilla = Fin | Registro Nombre Planilla 
    deriving Show

data Equipo = Becario Nombre 
            | Investigador Nombre Equipo Equipo Equipo
    deriving Show


> Ejercicio 1:

Reglas que definen el conjunto Planilla:
    > Regla base: Fin está en el conjunto Planilla.
    > Regla inductiva: Si n está en el conjunto Nombre, p está en el conjunto Planilla,
                       entonces Registro n p está en el conjunto Planilla.

Reglas que definen el conjunto Equipo:
    > Regla base: Becario n está en el conjunto Equipo, siendo que n está en el conjunto Nombre.
    > Regla inductiva: Si n está en el conjunto Nombre, e1 está en el conjunto Equipo, e2 está en el conjunto Equipo,
                       e3 está en el conjunto Equipo, entonces Investigador n e1 e2 e3 está en el conjunto Equipo.


> Ejercicio 2:

f :: Planilla -> a
f Fin            = ...
f (Registro n p) = ... n ... f p ...

f :: Equipo -> a
f (Becario n)               = ... n
f (Ingestigador n e1 e2 e3) = ... n ... f e1 ... f e2 ... f e3


> Ejercicio 3:

-- 3.A

largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin            = 0
largoDePlanilla (Registro _ p) = 1 + largoDePlanilla p


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
nivelesJerarquicos (Becario _)               = 0
nivelesJerarquicos (Investigador _ e1 e2 e3) = 1 + nivelesJerarquicos e1
                                                 + nivelesJerarquicos e2
                                                 + nivelesJerarquicos e3


-- 3.E

cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario _)               = 1
cantidadDeIntegrantes (Investigador _ e1 e2 e3) = 1 + cantidadDeIntegrantes e1
                                                    + cantidadDeIntegrantes e2
                                                    + cantidadDeIntegrantes e3


-- 3.F

planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n)               = Registro n Fin 
planillaDeIntegrantes (Investigador n e1 e2 e3) = Registro n (juntarPlanillas 
                                                                (juntarPlanillas (planillaDeIntegrantes e1)
                                                                                 (planillaDeIntegrantes e2))
                                                                (planillaDeIntegrantes e3))


> Ejercicio 4:

-- 4.A

Para todo p :: Planilla . largoDePlanilla (juntarPlanillas Fin p) = largoDePlanilla Fin + largoDePlanilla p

    -- LADO IZQUIERDO

        largoDePlanilla (juntarPlanillas Fin p)
    =                                                                   (juntarPlanillas.1) 
        largoDePlanilla p

    -- LADO DERECHO

        largoDePlanilla Fin + largoDePlanilla p
    =                                                                   (largoDePlanilla.1)
        0 + largoDePlanilla p
    =                                                                   (aritmética)
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
    =                                                                                               (juntarPlanillas.2)  
        largoDePlanilla (Registro "Alan" (juntarPlanillas (Registro "Edsger" Fin) p))
    =                                                                                               (juntarPlanillas.2)
        largoDePlanilla (Registro "Alan" (Registro "Edsger" (juntarPlanillas Fin p)))
    =                                                                                               (juntarPlanillas.1)
        largoDePlanilla (Registro "Alan" (Registro "Edsger" p))
    =                                                                                               (largoDePlanilla.2)
        1 + largoDePlanilla (Registro "Edsger" p)
    =                                                                                               (largoDePlanilla.2)
        1 + 1 + largoDePlanilla p
    =                                                                                               (aritmética)
        2 + largoDePlanilla p

    -- LADO DERECHO

        largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla p
    =                                                                                               (largoDePlanilla.2)
        1 + largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p
    =                                                                                               (largoDePlanilla.2)
        1 + 1 + largoDePlanilla Fin + largoDePlanilla p
    =                                                                                               (largoDePlanilla.1)
        1 + 1 + 0 + largoDePlanilla p
    =                                                                                               (aritmética)
        2 + largoDePlanilla p

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.D

Para todo p :: Planilla .
                    largoDePlanilla (juntarPlanillas (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) p) = 
                    largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Edsger" Fin))) + largoDePlanilla p

    -- LADO IZQUIERDO

        largoDePlanilla (juntarPlanillas
                            (Registro "Alonzo"
                                (Registro "Alan"
                                    (Registro "Edsger" Fin))) p)
    =                                                                                               (juntarPlanillas.2)
        largoDePlanilla (Registro "Alonzo"
                            (juntarPlanillas
                                (Registro "Alan"
                                    (Registro "Edsger" Fin)) p))
    =                                                                                               (juntarPlanillas.2)
        largoDePlanilla (Registro "Alonzo"
                            (Registro "Alan"
                                (juntarPlanillas
                                    (Registro "Edsger" Fin) p)))
    =                                                                                               (juntarPlanillas.2)
        largoDePlanilla (Registro "Alonzo"
                            (Registro "Alan"
                                (Registro "Edsger"
                                    (juntarPlanillas Fin p))))
    =                                                                                               (juntarPlanillas.1)
        largoDePlanilla (Registro "Alonzo"
                            (Registro "Alan"
                                (Registro "Edsger" p)))
    =                                                                                               (largoDePlanilla.2)
        1 + largoDePlanilla (Registro "Alan"
                                (Registro "Edsger" p))
    =                                                                                               (largoDePlanilla.2)
        1 + 1 + largoDePlanilla (Registro "Edsger" p)
    =                                                                                               (largoDePlanilla.2)
        1 + 1 + 1 + largoDePlanilla p
    =                                                                                               (aritmética)
        3 + largoDePlanilla p

    -- LADO DERECHO

        largoDePlanilla (Registro "Alonzo"
                            (Registro "Alan"
                                (Registro "Edsger" Fin))) + largoDePlanilla p
    =                                                                                               (largoDePlanilla.2)
        1 + largoDePlanilla (Registro "Alan"
                                (Registro "Edsger" Fin)) + largoDePlanilla p
    =                                                                                               (largoDePlanilla.2)
        1 + 1 + largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p
    =                                                                                               (largoDePlanilla.2)
        1 + 1 + 1 + largoDePlanilla Fin + largoDePlanilla p
    =                                                                                               (largoDePlanilla.1)
        1 + 1 + 1 + 0 + largoDePlanilla p
    =                                                                                               (aritmética)
        3 + largoDePlanilla p

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


> Ejercicio 5:

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

    -- Ambos lados llegan a lo mismo, la propiedad es válida.      


-- 5.D

largoDePlanilla (planillaDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))) = 
cantidadDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))

    -- LADO IZQUIERDO

        largoDePlanilla (planillaDeIntegrantes
                                (Investigador "Alonzo" (Becario "Alan")
                                (Becario "Alfred") (Becario "Stephen"))
                        )
    =                                                                                           (planillaDeIntegrantes.2)
        largoDePlanilla (Registro "Alonzo" 
                            (juntarPlanillas
                                (juntarPlanillas 
                                    (planillaDeIntegrantes (Becario "Alan"))
                                    (planillaDeIntegrantes (Becario "Alfred"))
                                ) 
                                (planillaDeIntegrantes (Becario "Stephen"))
                            )
                        )
    =                                                                                           (planillaDeIntegrantes.1)
        largoDePlanilla (Registro "Alonzo" 
                            (juntarPlanillas
                                (juntarPlanillas 
                                    (Registro "Alan" Fin)
                                    (planillaDeIntegrantes (Becario "Alfred"))
                                ) 
                                (planillaDeIntegrantes (Becario "Stephen"))
                            )
                        )
    =                                                                                           (planillaDeIntegrantes.1)
        largoDePlanilla (Registro "Alonzo"
                            (juntarPlanillas
                                (juntarPlanillas 
                                    (Registro "Alan" Fin)
                                    (Registro "Alfred" Fin)
                                ) 
                                (planillaDeIntegrantes (Becario "Stephen"))
                            )
                        )
    =                                                                                           (planillaDeIntegrantes.1)
        largoDePlanilla (Registro "Alonzo"
                            (juntarPlanillas
                                (juntarPlanillas 
                                    (Registro "Alan" Fin)
                                    (Registro "Alfred" Fin)
                                ) 
                                (Registro "Stephen" Fin)
                            )
                        )
    =                                                                                           (juntarPlanillas.2)
        largoDePlanilla (Registro "Alonzo"
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
    =                                                                                           (juntarPlanillas.1)
        largoDePlanilla (Registro "Alonzo"
                            (juntarPlanillas
                                (Registro "Alan" (Registro "Alfred" Fin)) 
                                (Registro "Stephen" Fin)
                            )
                        )
    =                                                                                           (juntarPlanillas.2)
        largoDePlanilla (Registro "Alonzo"
                            (Registro "Alan" 
                                (juntarPlanillas
                                    (Registro "Alfred" Fin)
                                    (Registro "Stephen" Fin)
                                )
                            )
                        )
    =                                                                                           (juntarPlanillas.2)
        largoDePlanilla (Registro "Alonzo"
                            (Registro "Alan" 
                                (Registro "Alfred" 
                                    (juntarPlanillas
                                        Fin
                                        (Registro "Stephen" Fin)
                                    )
                                )
                            )
                        )
    =                                                                                           (juntarPlanillas.1)
        largoDePlanilla (Registro "Alonzo"
                            (Registro "Alan"
                                (Registro "Alfred"
                                    (Registro "Stephen" Fin)
                                )
                            )
                        )
    =                                                                                           (largoDePlanilla.2)
        1 + largoDePlanilla (Registro "Alan" (Registro "Alfred" (Registro "Stephen" Fin)))
    =                                                                                           (largoDePlanilla.2)
        1 + 1 + largoDePlanilla (Registro "Alfred" (Registro "Stephen" Fin))
    =                                                                                           (largoDePlanilla.2)
        1 + 1 + 1 + largoDePlanilla (Registro "Stephen" Fin)
    =                                                                                           (largoDePlanilla.2)
        1 + 1 + 1 + 1 + largoDePlanilla Fin
    =                                                                                           (largoDePlanilla.1)
        1 + 1 + 1 + 1 + 0
    =                                                                                           (aritmética)
        4

    -- LADO DERECHO

        cantidadDeIntegrantes (Investigador "Alonzo" 
                                    (Becario "Alan")
                                    (Becario "Alfred")
                                    (Becario "Stephen")
                              )
    =                                                                                           (cantidadDeIntegrantes.2)
        1
        + cantidadDeIntegrantes (Becario "Alan")
        + cantidadDeIntegrantes (Becario "Alfred")
        + cantidadDeIntegrantes (Becario "Stephen")
    =                                                                                           (cantidadDeIntegrantes.1)
        1
        + 1
        + cantidadDeIntegrantes (Becario "Alfred")
        + cantidadDeIntegrantes (Becario "Stephen")
    =                                                                                           (cantidadDeIntegrantes.1)
        1 + 1 + 1 + cantidadDeIntegrantes (Becario "Stephen")
    =                                                                                           (cantidadDeIntegrantes.1)
        1 + 1 + 1 + 1
    =                                                                                           (aritmética)
        4

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 5.E

para todo n :: Nombre . para todo n1 :: Nombre . para todo n2 :: Nombre . para todo n3 :: Nombre . 
largoDePlanilla (planillaDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3))) =
cantidadDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3))

    -- LADO IZQUIERDO

        largoDePlanilla (planillaDeIntegrantes
                            (Investigador n
                                (Becario n1)
                                (Becario n2)
                                (Becario n3)
                            )
                        )
    =                                                                                               (planillaDeIntegrantes.2)
        largoDePlanilla (Registro n (juntarPlanillas 
                                        (juntarPlanillas 
                                            (planillaDeIntegrantes (Becario n1))
                                            (planillaDeIntegrantes (Becario n2))
                                        ) 
                                        (planillaDeIntegrantes (Becario n3))
                                    )
                        )
    =                                                                                               (planillaDeIntegrantes.1)
        largoDePlanilla (Registro n (juntarPlanillas 
                                        (juntarPlanillas 
                                            (Registro n1 Fin)
                                            (planillaDeIntegrantes (Becario n2))
                                        ) 
                                        (planillaDeIntegrantes (Becario n3))
                                    )
                        )
    =                                                                                               (planillaDeIntegrantes.1)
        largoDePlanilla (Registro n (juntarPlanillas 
                                        (juntarPlanillas 
                                            (Registro n1 Fin)
                                            (Registro n2 Fin)
                                        ) 
                                        (planillaDeIntegrantes (Becario n3))
                                    )
                        )
    =                                                                                               (planillaDeIntegrantes.1)
        largoDePlanilla (Registro n (juntarPlanillas 
                                        (juntarPlanillas 
                                            (Registro n1 Fin)
                                            (Registro n2 Fin)
                                        ) 
                                        (Registro n3 Fin)
                                    )
                        )
    =                                                                                               (juntarPlanillas.2)
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
    =                                                                                               (juntarPlanillas.1)
        largoDePlanilla (Registro n (juntarPlanillas 
                                        (Registro n1 (Registro n2 Fin)) 
                                        (Registro n3 Fin)
                                    )
                        )
    =                                                                                               (juntarPlanillas.2)
        largoDePlanilla (Registro n (Registro n1
                                        (juntarPlanillas
                                            (Registro n2 Fin)
                                            (Registro n3 Fin)
                                        )
                                    )
                        )
    =                                                                                               (juntarPlanillas.2)
        largoDePlanilla (Registro n (Registro n1
                                        (Registrar n2
                                            (juntarPlanillas
                                                Fin
                                                (Registro n3 Fin)
                                            )
                                        )
                                    )
                        )
    =                                                                                               (juntarPlanillas.1)
        largoDePlanilla (Registro n (Registro n1 (Registro n2 (Registro n3 Fin))))
    =                                                                                               (largoDePlanilla.2)
        1 + largoDePlanilla (Registro n1 (Registro n2 (Registro n3 Fin)))
    =                                                                                               (largoDePlanilla.2)
        1 + 1 + largoDePlanilla (Registro n2 (Registro n3 Fin))
    =                                                                                               (largoDePlanilla.2)
        1 + 1 + 1 + largoDePlanilla (Registro n3 Fin)
    =                                                                                               (largoDePlanilla.2)
        1 + 1 + 1 + 1 + largoDePlanilla Fin
    =                                                                                               (largoDePlanilla.1)
        1 + 1 + 1 + 1 + 0
    =                                                                                               (aritmética)
        4

        cantidadDeIntegrantes (Investigador n (Becario n1) (Becario n2) (Becario n3))
    =                                                                                               (cantidadDeIntegrantes.2)
        1
        + cantidadDeIntegrantes (Becario n1)
        + cantidadDeIntegrantes (Becario n2)
        + cantidadDeIntegrantes (Becario n3)
    =                                                                                               (cantidadDeIntegrantes.1)
        1
        + 1
        + cantidadDeIntegrantes (Becario n2)
        + cantidadDeIntegrantes (Becario n3)
    =                                                                                               (cantidadDeIntegrantes.1)
        1
        + 1
        + 1
        + cantidadDeIntegrantes (Becario n3)
    =                                                                                               (cantidadDeIntegrantes.1)
        1 + 1 + 1 + 1
    =                                                                                               (aritmética)
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
    =                                                                                               (planillaDeIntegrantes.2)
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
    =                                                                                               (planillaDeIntegrantes.2)
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
    =                                                                                               (planillaDeIntegrantes.1)
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
    =                                                                                               (planillaDeIntegrantes.1)
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
    =                                                                                               (planillaDeIntegrantes.1)
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
    =                                                                                               (juntarPlanillas.2)
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
    =                                                                                               (juntarPlanillas.1)
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
    =                                                                                               (juntarPlanillas.2)
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
    =                                                                                               (juntarPlanillas.2)
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
    =                                                                                               (juntarPlanillas.1)
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
                                            (Registro "Raymond" Fin))))
                            )
                        )
    =                                                                                               (planillaDeIntegrantes.2)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (planillaDeIntegrantes.2)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (planillaDeIntegrantes.1)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (planillaDeIntegrantes.1)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (planillaDeIntegrantes.1)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (planillaDeIntegrantes.1)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (planillaDeIntegrantes.1)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (planillaDeIntegrantes.1)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (juntarPlanillas.2)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (juntarPlanillas.1)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (juntarPlanillas.2)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (juntarPlanillas.1)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (juntarPlanillas.2)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (juntarPlanillas.2)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (juntarPlanillas.2)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (juntarPlanillas.2)
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (juntarPlanillas.1)
        largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                (juntarPlanillas 
                                    (Registro "Alonzo"
                                        (Registro "Alan"
                                            (Registro "Alfred"
                                                (Registro "Stephen" Fin)))) 
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
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (juntarPlanillas.1)
        largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                (juntarPlanillas 
                                        (Registro "Alonzo"
                                            (Registro "Alan"
                                                (Registro "Alfred"
                                                    (Registro "Stephen" Fin)))) 
                                        (Registro "John"
                                            (Registro "Brian"
                                                (Registro "Graham"
                                                    (Registro "Ioan" Fin)))) 
                                )
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (juntarPlanillas.2)
        largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                (Registro "Alonzo"
                                    (juntarPlanillas
                                        (Registro "Alan"
                                            (Registro "Alfred"
                                                (Registro "Stephen" Fin)))
                                        (Registro "John"
                                            (Registro "Brian"
                                                (Registro "Graham"
                                                    (Registro "Ioan" Fin))))
                                    ) 
                                )
                                (Registro "Robert"
                                    (Registro "Gordon" (Registro "John"
                                        (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (juntarPlanillas.2)
        largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                (Registro "Alonzo"
                                    (Registro "Alan" 
                                        (juntarPlanillas
                                            (Registro "Alfred"
                                                (Registro "Stephen" Fin))
                                            (Registro "John"
                                                (Registro "Brian"
                                                    (Registro "Graham"
                                                        (Registro "Ioan" Fin))))
                                        )
                                    ) 
                                )
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (juntarPlanillas.2)
        largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                (Registro "Alonzo"
                                    (Registro "Alan" 
                                        (Registro "Alfred"
                                            (juntarPlanillas
                                                (Registro "Stephen" Fin)
                                                (Registro "John"
                                                    (Registro "Brian"
                                                        (Registro "Graham"
                                                            (Registro "Ioan" Fin))))
                                            )
                                        )
                                    ) 
                                )
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (juntarPlanillas.2)
        largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                (Registro "Alonzo"
                                    (Registro "Alan" 
                                        (Registro "Alfred"
                                            (Registro "Stephen"
                                                (juntarPlanillas
                                                    Fin
                                                    (Registro "John"
                                                        (Registro "Brian"
                                                            (Registro "Graham"
                                                                (Registro "Ioan" Fin))))
                                                )
                                            )
                                        )
                                    ) 
                                )
                                (Registro "Robert"
                                    (Registro "Gordon"
                                        (Registro "John"
                                            (Registro "Raymond" Fin))))
                            ) 
                        )
    =                                                                                               (juntarPlanillas.2)
        largoDePlanilla (Registro "Oswald"
                            (juntarPlanillas 
                                (Registro "Alonzo"
                                    (Registro "Alan" 
                                        (Registro "Alfred"
                                            (Registro "Stephen"
                                                    (Registro "John"
                                                        (Registro "Brian"
                                                            (Registro "Graham"
                                                                (Registro "Ioan" Fin))))
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
    =                                                                                               (juntarPlanillas.2)
        largoDePlanilla (Registro "Oswald"
                            (Registro "Alonzo"
                                (juntarPlanillas
                                    (Registro "Alan" 
                                        (Registro "Alfred"
                                            (Registro "Stephen"
                                                (Registro "John"
                                                    (Registro "Brian"
                                                        (Registro "Graham"
                                                            (Registro "Ioan" Fin))))
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
    =                                                                                               (juntarPlanillas.2)
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
    =                                                                                               (juntarPlanillas.2)
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
    =                                                                                               (juntarPlanillas.2)
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
    =                                                                                               (juntarPlanillas.2)
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
    =                                                                                               (juntarPlanillas.2)
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
    =                                                                                               (juntarPlanillas.2)
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
    =                                                                                               (juntarPlanillas.2)
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
    =                                                                                               (juntarPlanillas.2)
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
    =                                                                                               (largoDePlanilla.2)
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
    =                                                                                               (largoDePlanilla.2)
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
    =                                                                                               (largoDePlanilla.2)
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
    =                                                                                               (largoDePlanilla.2)
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
    =                                                                                               (largoDePlanilla.2)
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
    =                                                                                               (largoDePlanilla.2)
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
    =                                                                                               (largoDePlanilla.2)
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
    =                                                                                               (largoDePlanilla.2)
        1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + largoDePlanilla (Registro "Ioan"
                                                            (Registro "Robert" 
                                                                (Registro "Gordon" 
                                                                    (Registro "John"
                                                                        (Registro "Raymond" Fin)
                                                                    )
                                                                )
                                                            )
                                                        )
    =                                                                                               (largoDePlanilla.2)
        1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + largoDePlanilla (Registro "Robert" 
                                                                (Registro "Gordon" 
                                                                    (Registro "John"
                                                                        (Registro "Raymond" Fin)
                                                                    )
                                                                )
                                                            )
    =                                                                                               (largoDePlanilla.2)
        1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + largoDePlanilla (Registro "Gordon" 
                                                                    (Registro "John"
                                                                        (Registro "Raymond" Fin)
                                                                    )
                                                                )
    =                                                                                               (largoDePlanilla.2)
        1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + largoDePlanilla (Registro "John"
                                                                        (Registro "Raymond" Fin)
                                                                    )
    =                                                                                               (largoDePlanilla.2)
        1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + largoDePlanilla (Registro "Raymond" Fin)
    =                                                                                               (largoDePlanilla.2)
        1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + largoDePlanilla Fin
    =                                                                                               (largoDePlanilla.1)
        1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 0
    =                                                                                               (aritmética)
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
    =                                                                                               (cantidadDeIntegrantes.2)
        1
        + cantidadDeIntegrantes (Investigador "Alonzo" 
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
    =                                                                                               (cantidadDeIntegrantes.2)
        1
        + 1
        + cantidadDeIntegrantes (Becario "Alan")
        + cantidadDeIntegrantes (Becario "Alfred")
        + cantidadDeIntegrantes (Becario "Stephen")
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
    =                                                                                               (cantidadDeIntegrantes.2)
        1
        + 1
        + cantidadDeIntegrantes (Becario "Alan")
        + cantidadDeIntegrantes (Becario "Alfred")
        + cantidadDeIntegrantes (Becario "Stephen")
        + cantidadDeIntegrantes (Becario "Brian")
        + cantidadDeIntegrantes (Becario "Graham")
        + cantidadDeIntegrantes (Becario "Ioan")
        + cantidadDeIntegrantes (Investigador "Robert" 
                                    (Becario "Gordon") 
                                    (Becario "John") 
                                    (Becario "Raymond")
                                )
    =                                                                                               (cantidadDeIntegrantes.2)
        1
        + 1
        + cantidadDeIntegrantes (Becario "Alan")
        + cantidadDeIntegrantes (Becario "Alfred")
        + cantidadDeIntegrantes (Becario "Stephen")
        + 1
        + cantidadDeIntegrantes (Becario "Brian")
        + cantidadDeIntegrantes (Becario "Graham")
        + cantidadDeIntegrantes (Becario "Ioan")
        + 1
        + cantidadDeIntegrantes (Becario "Gordon")
        + cantidadDeIntegrantes (Becario "John")
        + cantidadDeIntegrantes (Becario "Raymond")
    =                                                                                               (cantidadDeIntegrantes.1)
        1
        + 1
        + 1
        + cantidadDeIntegrantes (Becario "Alfred")
        + cantidadDeIntegrantes (Becario "Stephen")
        + 1
        + cantidadDeIntegrantes (Becario "Brian")
        + cantidadDeIntegrantes (Becario "Graham")
        + cantidadDeIntegrantes (Becario "Ioan")
        + 1
        + cantidadDeIntegrantes (Becario "Gordon")
        + cantidadDeIntegrantes (Becario "John")
        + cantidadDeIntegrantes (Becario "Raymond")
    =                                                                                               (cantidadDeIntegrantes.1)
        1
        + 1
        + 1
        + 1
        + cantidadDeIntegrantes (Becario "Stephen")
        + 1
        + cantidadDeIntegrantes (Becario "Brian")
        + cantidadDeIntegrantes (Becario "Graham")
        + cantidadDeIntegrantes (Becario "Ioan")
        + 1
        + cantidadDeIntegrantes (Becario "Gordon")
        + cantidadDeIntegrantes (Becario "John")
        + cantidadDeIntegrantes (Becario "Raymond")
    =                                                                                               (cantidadDeIntegrantes.1)
        1
        + 1
        + 1
        + 1
        + 1
        + 1
        + cantidadDeIntegrantes (Becario "Brian")
        + cantidadDeIntegrantes (Becario "Graham")
        + cantidadDeIntegrantes (Becario "Ioan")
        + 1
        + cantidadDeIntegrantes (Becario "Gordon")
        + cantidadDeIntegrantes (Becario "John")
        + cantidadDeIntegrantes (Becario "Raymond")
    =                                                                                               (cantidadDeIntegrantes.1)
        1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + cantidadDeIntegrantes (Becario "Graham")
        + cantidadDeIntegrantes (Becario "Ioan")
        + 1
        + cantidadDeIntegrantes (Becario "Gordon")
        + cantidadDeIntegrantes (Becario "John")
        + cantidadDeIntegrantes (Becario "Raymond")
    =                                                                                               (cantidadDeIntegrantes.1)
        1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + cantidadDeIntegrantes (Becario "Ioan")
        + 1
        + cantidadDeIntegrantes (Becario "Gordon")
        + cantidadDeIntegrantes (Becario "John")
        + cantidadDeIntegrantes (Becario "Raymond")
    =                                                                                               (cantidadDeIntegrantes.1)
        1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + cantidadDeIntegrantes (Becario "Gordon")
        + cantidadDeIntegrantes (Becario "John")
        + cantidadDeIntegrantes (Becario "Raymond")
    =                                                                                               (cantidadDeIntegrantes.1)
        1 +
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + cantidadDeIntegrantes (Becario "John")
        + cantidadDeIntegrantes (Becario "Raymond")
    =                                                                                               (cantidadDeIntegrantes.1)
        1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
        + cantidadDeIntegrantes (Becario "Raymond")
    =                                                                                               (cantidadDeIntegrantes.1)
        1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
    =                                                                                               (aritmética)
        13

    -- Ambos lados llegan a lo mismo, la propiedad es válida.

#############################################################################################################################

## SECCIÓN 3:

data Dungeon a = Habitacion a 
               | Pasaje (Maybe a) (Dungeon a) 
               | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a)
    deriving Show


> Ejercicio 1:

Reglas que definen el conjunto Dungeon a:
    > Regla base: Habitacion a está en el conjunto Dungeon a.
    > Regla inductiva 1: Si m está en el conjunto Maybe a, d está en el conjunto Dungeon a,
                         entonces Pasaje m d está en el conjunto Dungeon a.
    > Regla inductiva 2: Si m está en el conjunto Maybe a, d1 está en el conjunto Dungeon a, d2 está en el
                         conjunto Dungeon a, entonces Bifurcacion m d1 d2 está en el conjunto Dungeon a.


> Ejercicio 2:

f :: Dungeon a -> b
f (Habitacion n)        = ... n
f (Pasaje m d)          = ... m ... f d
f (Bifurcacion m d1 d2) = ... m ... f d1 ... f d2


> Ejercicio 3:

-- 3.A

cantidadDeBifurcaciones :: Dungeon a -> Int
cantidadDeBifurcaciones (Habitacion _)        = 0
cantidadDeBifurcaciones (Pasaje _ d)          = cantidadDeBifurcaciones d
cantidadDeBifurcaciones (Bifurcacion _ d1 d2) = 1 + cantidadDeBifurcaciones d1 + cantidadDeBifurcaciones d2


-- 3.B

cantidadDePuntosInteresantes :: Dungeon a -> Int
cantidadDePuntosInteresantes (Habitacion _)        = 0
cantidadDePuntosInteresantes (Pasaje _ d)          = 1 + cantidadDePuntosInteresantes d
cantidadDePuntosInteresantes (Bifurcacion _ d1 d2) = 1 + cantidadDePuntosInteresantes d1 + cantidadDePuntosInteresantes d2


-- 3.C

cantidadDePuntosVacios :: Dungeon a -> Int
cantidadDePuntosVacios (Habitacion _)        = 0
cantidadDePuntosVacios (Pasaje m d)          = unoSi (estaVacioM m) + cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion m d1 d2) = unoSi (estaVacioM m) + cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

estaVacioM :: Maybe a -> Bool
estaVacioM Nothing = True
estaVacioM _       = False 


-- 3.D

cantidadDePuntosCon :: Eq a => a -> Dungeon a -> Int
cantidadDePuntosCon x' (Habitacion x)        = unoSi (x == x')
cantidadDePuntosCon x' (Pasaje m d)          = unoSi (tieneM m x') + cantidadDePuntosCon x' d
cantidadDePuntosCon x' (Bifurcacion m d1 d2) = unoSi (tieneM m x') + cantidadDePuntosCon x' d1 + cantidadDePuntosCon x' d2

tieneM :: Eq a => Maybe a -> a -> Bool
tieneM Nothing  _  = False
tieneM (Just x) x' = x == x'


-- 3.E

esLineal :: Dungeon a -> Bool
esLineal (Habitacion _)        = True
esLineal (Pasaje _ d)          = esLineal d
esLineal (Bifurcacion _ d1 d2) = False


-- 3.F

llenoDe :: Eq a => a -> Dungeon a -> Bool
llenoDe x' (Habitacion x)        = mismoEnM (Just x) x'
llenoDe x' (Pasaje m d)          = mismoEnM m x' && llenoDe x' d
llenoDe x' (Bifurcacion m d1 d2) = mismoEnM m x' && llenoDe x' d1 && llenoDe x' d2

mismoEnM :: Eq a => Maybe a -> a -> Bool
mismoEnM Nothing  _  = False
mismoEnM (Just x) x' = x == x'


> Ejercicio 4:

data Tesoro = Cofre | Oro | Joyas
    deriving Show


-- 4.A

Para todo x :: a . cantidadDePuntosVacios (Habitacion x) = 0

    -- LADO IZQUIERDO

        cantidadDePuntosVacios (Habitacion x)
    =                                               (cantidadDePuntosVacios.1)
        0

    -- LADO DERECHO

        0

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.B

cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas)) = 1

    -- LADO IZQUIERDO

        cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
    =                                                                           (cantidadDePuntosVacios.2)
        unoSi (estaVacioM Nothing) + cantidadDePuntosVacios (Habitacion Joyas)
    =                                                                           (cantidadDePuntosVacios.1)
        unoSi (estaVacioM Nothing) + 0
    =                                                                           (estaVacioM.1)
        unoSi True + 0
    =                                                                           (unoSi.1)
        1 + 0
    =                                                                           (aritmética)
        1

    -- LADO DERECHO

        1

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.C

Para todo y :: a . para todo x :: a . cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x)) = 0

    -- LADO IZQUIERDO

        cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x))
    =                                                                           (cantidadDePuntosVacios.2)
        unoSi (estaVacioM (Just y)) + cantidadDePuntosVacios (Habitacion x)
    =                                                                           (cantidadDePuntosVacios.1)
        unoSi (estaVacioM (Just y)) + 0
    =                                                                           (estaVacioM.2)
        unoSi False + 0
    =                                                                           (unoSi.2)
        0 + 0
    =                                                                           (aritmética)
        0

    -- LADO DERECHO

        0

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.D

cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing (Habitacion Joyas)) (Pasaje (Just Oro) (Habitacion Cofre))) = 2

    -- LADO IZQUIERDO

        cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing (Habitacion Joyas)) 
                                                    (Pasaje (Just Oro) (Habitacion Cofre)))
    =                                                                                               (cantidadDePuntosVacios.3)
        unoSi (estaVacioM Nothing)
        + cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas)) 
        + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Cofre))
    =                                                                                               (cantidadDePuntosVacios.2)
        unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM Nothing)
        + cantidadDePuntosVacios (Habitacion Joyas)
        + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Cofre))
    =                                                                                               (cantidadDePuntosVacios.1)
        unoSi (estaVacioM Nothing) 
        + unoSi (estaVacioM Nothing)
        + 0
        + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Cofre))
    =                                                                                               (cantidadDePuntosVacios.2)
        unoSi (estaVacioM Nothing) 
        + unoSi (estaVacioM Nothing)
        + 0
        + unoSi (estaVacioM (Just Oro))
        + cantidadDePuntosVacios (Habitacion Cofre)
    =                                                                                               (cantidadDePuntosVacios.1)
        unoSi (estaVacioM Nothing) 
        + unoSi (estaVacioM Nothing)
        + 0
        + unoSi (estaVacioM (Just Oro))
        + 0
    =                                                                                               (estaVacioM.1)
        unoSi True 
        + unoSi (estaVacioM Nothing)
        + 0
        + unoSi (estaVacioM (Just Oro))
        + 0
    =                                                                                               (estaVacioM.1)
        unoSi True 
        + unoSi True
        + 0
        + unoSi (estaVacioM (Just Oro))
        + 0
    =                                                                                               (estaVacioM.2)
        unoSi True + unoSi True + 0 + unoSi False + 0
    =                                                                                               (unoSi.1)
        1 + unoSi True + 0 + unoSi False + 0
    =                                                                                               (unoSi.1)
        1 + 1 + 0 + unoSi False + 0
    =                                                                                               (unoSi.2)
        1 + 1 + 0 + 0 + 0
    =                                                                                               (aritmética)
        2

    -- LADO DERECHO

        2

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.E

Para todo z :: a . para todo y :: a . para todo x :: a . 
    cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing  (Habitacion z)) (Pasaje (Just y) (Habitacion x))) = 2

    -- LADO IZQUIERDO

        cantidadDePuntosVacios (Bifurcacion Nothing 
                                    (Pasaje Nothing  (Habitacion z))
                                    (Pasaje (Just y) (Habitacion x)))
    =                                                                                           (cantidadDePuntosVacios.3)
        unoSi (estaVacioM Nothing)
        + cantidadDePuntosVacios (Pasaje Nothing (Habitacion z))
        + cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x))
    =                                                                                           (cantidadDePuntosVacios.2)
        unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM Nothing)
        + cantidadDePuntosVacios (Habitacion z)
        + cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x))
    =                                                                                           (cantidadDePuntosVacios.1)
        unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM Nothing)
        + 0
        + cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x))
    =                                                                                           (cantidadDePuntosVacios.2)
        unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM Nothing)
        + 0
        + unoSi (estaVacioM (Just y))
        + cantidadDePuntosVacios (Habitacion x)
    =                                                                                           (cantidadDePuntosVacios.1)
        unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM Nothing)
        + 0
        + unoSi (estaVacioM (Just y))
        + 0
    =                                                                                           (estaVacioM.1)
        unoSi True
        + unoSi (estaVacioM Nothing)
        + 0
        + unoSi (estaVacioM (Just y))
        + 0
    =                                                                                           (estaVacioM.1)
        unoSi True + unoSi True + 0 + unoSi (estaVacioM (Just y)) + 0
    =                                                                                           (estaVacioM.2)
        unoSi True + unoSi True + 0 + unoSi False + 0
    =                                                                                           (unoSi.1)
        1 + unoSi True + 0 + unoSi False + 0
    =                                                                                           (unoSi.1)
        1 + 1 + 0 + unoSi False + 0
    =                                                                                           (unoSi.2)
        1 + 1 + 0 + 0 + 0
    =                                                                                           (aritmética)
        2

    -- LADO DERECHO

        2

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.F

cantidadDePuntosVacios (Bifurcacion (Just Cofre) (Bifurcacion Nothing 
                                                    (Pasaje Nothing    (Habitacion Joyas)) 
                                                    (Pasaje (Just Oro) (Habitacion Cofre))
                                                 ) 
                                                 (Bifurcacion Nothing 
                                                    (Pasaje (Just Oro) (Habitacion Oro))
                                                    (Pasaje Nothing    (Habitacion Joyas))
                                                 ))
= 4

    -- LADO IZQUIERDO

        cantidadDePuntosVacios 
            (Bifurcacion (Just Cofre) 
                (Bifurcacion Nothing 
                    (Pasaje Nothing (Habitacion Joyas)) 
                    (Pasaje (Just Oro) (Habitacion Cofre))
                ) 
                (Bifurcacion Nothing 
                    (Pasaje (Just Oro) (Habitacion Oro))
                    (Pasaje Nothing (Habitacion Joyas))
                )
            )
    =                                                                               (cantidadDePuntosVacios.3)
        unoSi (estaVacioM (Just Cofre))
        + cantidadDePuntosVacios 
            (Bifurcacion Nothing 
                (Pasaje Nothing (Habitacion Joyas)) 
                (Pasaje (Just Oro) (Habitacion Cofre))
            )
        + cantidadDePuntosVacios
            (Bifurcacion Nothing 
                (Pasaje (Just Oro) (Habitacion Oro))
                (Pasaje Nothing (Habitacion Joyas))
            )
    =                                                                               (cantidadDePuntosVacios.3)
        unoSi (estaVacioM (Just Cofre))
        + unoSi (estaVacioM Nothing)
        + cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
        + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Cofre))
        + cantidadDePuntosVacios
            (Bifurcacion Nothing 
                (Pasaje (Just Oro) (Habitacion Oro))
                (Pasaje Nothing (Habitacion Joyas))
            )
    =                                                                               (cantidadDePuntosVacios.3)
        unoSi (estaVacioM (Just Cofre))
        + unoSi (estaVacioM Nothing)
        + cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
        + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Cofre))
        + unoSi (estaVacioM Nothing)
        + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Oro))
        + cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
    =                                                                               (cantidadDePuntosVacios.2)
        unoSi (estaVacioM (Just Cofre))
        + unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM Nothing)
        + cantidadDePuntosVacios (Habitacion Joyas)
        + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Cofre))
        + unoSi (estaVacioM Nothing)
        + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Oro))
        + cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
    =                                                                               (cantidadDePuntosVacios.2)
        unoSi (estaVacioM (Just Cofre))
        + unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM Nothing)
        + cantidadDePuntosVacios (Habitacion Joyas)
        + unoSi (estaVacioM (Just Oro))
        + cantidadDePuntosVacios (Habitacion Cofre)
        + unoSi (estaVacioM Nothing)
        + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Oro))
        + cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
    =                                                                               (cantidadDePuntosVacios.1)
        unoSi (estaVacioM (Just Cofre))
        + unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM Nothing)
        + 0
        + unoSi (estaVacioM (Just Oro))
        + cantidadDePuntosVacios (Habitacion Cofre)
        + unoSi (estaVacioM Nothing)
        + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Oro))
        + cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
    =                                                                               (cantidadDePuntosVacios.1)
        unoSi (estaVacioM (Just Cofre))
        + unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM Nothing)
        + 0
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi (estaVacioM Nothing)
        + cantidadDePuntosVacios (Pasaje (Just Oro) (Habitacion Oro))
        + cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
    =                                                                               (cantidadDePuntosVacios.2)
        unoSi (estaVacioM (Just Cofre))
        + unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM Nothing)
        + 0
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM (Just Oro))
        + cantidadDePuntosVacios (Habitacion Oro)
        + cantidadDePuntosVacios (Pasaje Nothing (Habitacion Joyas))
    =                                                                               (cantidadDePuntosVacios.2)
        unoSi (estaVacioM (Just Cofre))
        + unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM Nothing)
        + 0
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM (Just Oro))
        + cantidadDePuntosVacios (Habitacion Oro)
        + unoSi (estaVacioM Nothing)
        + cantidadDePuntosVacios (Habitacion Joyas)
    =                                                                               (cantidadDePuntosVacios.1)
        unoSi (estaVacioM (Just Cofre))
        + unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM Nothing)
        + 0
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi (estaVacioM Nothing)
        + cantidadDePuntosVacios (Habitacion Joyas)
    =                                                                               (cantidadDePuntosVacios.1)
        unoSi (estaVacioM (Just Cofre))
        + unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM Nothing)
        + 0
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi (estaVacioM Nothing)
        + 0
    =                                                                               (estaVacioM.1)
        unoSi (estaVacioM (Just Cofre))
        + unoSi True
        + unoSi (estaVacioM Nothing)
        + 0
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi (estaVacioM Nothing)
        + 0
    =                                                                               (estaVacioM.1)
        unoSi (estaVacioM (Just Cofre))
        + unoSi True
        + unoSi True
        + 0
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi (estaVacioM Nothing)
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi (estaVacioM Nothing)
        + 0
    =                                                                               (estaVacioM.1)
        unoSi (estaVacioM (Just Cofre))
        + unoSi True
        + unoSi True
        + 0
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi True
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi (estaVacioM Nothing)
        + 0
    =                                                                               (estaVacioM.1)
        unoSi (estaVacioM (Just Cofre))
        + unoSi True
        + unoSi True
        + 0
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi True
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi True
        + 0
    =                                                                               (estaVacioM.2)
        unoSi False
        + unoSi True
        + unoSi True
        + 0
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi True
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi True
        + 0
    =                                                                               (estaVacioM.2)
        unoSi False
        + unoSi True
        + unoSi True
        + 0
        + unoSi False
        + 0
        + unoSi True
        + unoSi (estaVacioM (Just Oro))
        + 0
        + unoSi True
        + 0
    =                                                                               (estaVacioM.2)
        unoSi False
        + unoSi True
        + unoSi True
        + 0
        + unoSi False
        + 0
        + unoSi True
        + unoSi False
        + 0
        + unoSi True
        + 0
    =                                                                               (unoSi.1)
        unoSi False
        + 1
        + unoSi True
        + 0
        + unoSi False
        + 0
        + unoSi True
        + unoSi False
        + 0
        + unoSi True
        + 0
    =                                                                               (unoSi.1)
        unoSi False
        + 1
        + 1
        + 0
        + unoSi False
        + 0
        + unoSi True
        + unoSi False
        + 0
        + unoSi True
        + 0
    =                                                                               (unoSi.1)
        unoSi False
        + 1
        + 1
        + 0
        + unoSi False
        + 0
        + 1
        + unoSi False
        + 0
        + unoSi True
        + 0
    =                                                                               (unoSi.1)
        unoSi False
        + 1
        + 1
        + 0
        + unoSi False
        + 0
        + 1
        + unoSi False
        + 0
        + 1
        + 0
    =                                                                               (unoSi.0)
        unoSi False
        + 1
        + 1
        + 0
        + unoSi False
        + 0
        + 1
        + unoSi False
        + 0
        + 1
        + 0
    =                                                                               (unoSi.0)
        0
        + 1
        + 1
        + 0
        + unoSi False
        + 0
        + 1
        + unoSi False
        + 0
        + 1
        + 0
    =                                                                               (unoSi.0)
        0
        + 1
        + 1
        + 0
        + 0
        + 0
        + 1
        + unoSi False
        + 0
        + 1
        + 0
    =                                                                               (unoSi.0)
        0
        + 1
        + 1
        + 0
        + 0
        + 0
        + 1
        + 0
        + 0
        + 1
        + 0
    =                                                                               (aritmética)
        4

    -- LADO DERECHO

        4

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


> Ejercicio 5:

data VariasCosas a b = Objeto a | Criatura b
    deriving Show

data Monstruo = Gargola | Dragon | Troll
    deriving Show


-- 5.A

cantidadDePuntosCon (Criatura Troll) (Habitacion (Objeto Oro)) = 0 

    -- LADO IZQUIERDO

        cantidadDePuntosCon (Criatura Troll) (Habitacion (Objeto Oro))
    =                                                                           (cantidadDePuntosCon.1)
        unoSi ((Criatura Troll) == (Objeto Oro))
    =                                                                           (==)
        unoSi False
    =                                                                           (unoSi.2)
        0

    -- LADO DERECHO

        0

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 5.B

cantidadDePuntosCon (Criatura Troll) (Pasaje (Just (Criatura Troll)) (Habitacion (Objeto Oro))) = 1 

    -- LADO IZQUIERDO

        cantidadDePuntosCon 
            (Criatura Troll)
                (Pasaje (Just (Criatura Troll))
                    (Habitacion (Objeto Oro)))
    =                                                                                          (cantidadDePuntosCon.2)
        unoSi (tieneM (Just (Criatura Troll)) (Criatura Troll))
        + cantidadDePuntosCon (Criatura Troll) (Habitacion (Objeto Oro))
    =                                                                                          (cantidadDePuntosCon.1)
        unoSi (tieneM (Just (Criatura Troll)) (Criatura Troll))
        + unoSi ((Criatura Troll) == (Objeto Oro))
    =                                                                                          (==)
        unoSi (tieneM (Just (Criatura Troll)) (Criatura Troll)) + unoSi False
    =                                                                                          (unoSi.2)
        unoSi (tieneM (Just (Criatura Troll)) (Criatura Troll)) + 0
    =                                                                                          (tieneM.2)
        unoSi ((Criatura Troll) == (Criatura Troll)) + 0
    =                                                                                          (==)
        unoSi True + 0
    =                                                                                          (unoSi.1)
        1 + 0
    =                                                                                          (aritmética)
        1

    -- LADO DERECHO

        1

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 5.C

cantidadDePuntosCon (Criatura Troll) (Pasaje (Just (Criatura Troll)) (Habitacion (Criatura Troll))) = 2 

    -- LADO IZQUIERDO

        cantidadDePuntosCon (Criatura Troll)
                                (Pasaje (Just (Criatura Troll))
                                    (Habitacion (Criatura Troll)))
    =                                                                                           (cantidadDePuntosCon.2)
        unoSi (tieneM (Just (Criatura Troll)) (Criatura Troll))
        + cantidadDePuntosCon (Criatura Troll) (Habitacion (Criatura Troll))
    =                                                                                           (cantidadDePuntosCon.1)
        unoSi (tieneM (Just (Criatura Troll)) (Criatura Troll))
        + unoSi ((Criatura Troll) == (Criatura Troll))
    =                                                                                           (==)
        unoSi (tieneM (Just (Criatura Troll)) (Criatura Troll))
        + unoSi True
    =                                                                                           (unoSi.1)
        unoSi (tieneM (Just (Criatura Troll)) (Criatura Troll)) + 1
    =                                                                                           (tieneM.2)
        unoSi ((Criatura Troll) == (Criatura Troll)) + 1
    =                                                                                           (==)
        unoSi True + 1
    =                                                                                           (unoSi.1)
        1 + 1
    =                                                                                           (aritmética)
        2

    -- LADO DERECHO

        2

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 5.D

cantidadDePuntosCon (Criatura Troll) 
                        (Bifurcacion (Just (Criatura Troll))
                            (Pasaje (Just (Criatura Troll)) 
                                    (Habitacion (Objeto Oro))
                            ) 
                            (Pasaje (Just (Criatura Troll)) 
                                    (Habitacion (Criatura Troll))
                            )
                        ) = 4 

    -- LADO IZQUIERDO

        cantidadDePuntosCon (Criatura Troll) 
                                (Bifurcacion (Just (Criatura Troll))
                                    (Pasaje (Just (Criatura Troll)) 
                                            (Habitacion (Objeto Oro))
                                    ) 
                                    (Pasaje (Just (Criatura Troll)) 
                                            (Habitacion (Criatura Troll))
                                    )
                            )
    =                                                                                               (cantidadDePuntosCon.3)
        unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + cantidadDePuntosCon (Criatura Troll)
                                (Pasaje (Just (Criatura Troll))
                                (Habitacion (Objeto Oro)))
        + cantidadDePuntosCon (Criatura Troll)
                                (Pasaje (Just (Criatura Troll))
                                (Habitacion (Criatura Troll)))
    =                                                                                               (cantidadDePuntosCon.2)
        unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + cantidadDePuntosCon (Criatura Troll) (Habitacion (Objeto Oro))
        + cantidadDePuntosCon (Criatura Troll)
                                (Pasaje (Just (Criatura Troll))
                                (Habitacion (Criatura Troll)))
    =                                                                                               (cantidadDePuntosCon.1)
        unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi ((Criatura Troll) == (Objeto Oro))
        + cantidadDePuntosCon (Criatura Troll)
                                (Pasaje (Just (Criatura Troll))
                                (Habitacion (Criatura Troll)))
    =                                                                                               (cantidadDePuntosCon.2)
        unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi ((Criatura Troll) == (Objeto Oro))
        + unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + cantidadDePuntosCon (Criatura Troll) (Habitacion (Criatura Troll))
    =                                                                                               (cantidadDePuntosCon.1)
        unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi ((Criatura Troll) == (Objeto Oro))
        + unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi ((Criatura Troll) == (Criatura Troll))
    =                                                                                               (==)
        unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi False
        + unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi ((Criatura Troll) == (Criatura Troll))
    =                                                                                               (==)
        unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi False
        + unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi True
    =                                                                                               (tieneM.2)
        unoSi ((Criatura Troll) == (Criatura Troll))
        + unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi False
        + unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi True
    =                                                                                               (tieneM.2)
        unoSi ((Criatura Troll) == (Criatura Troll))
        + unoSi ((Criatura Troll) == (Criatura Troll))
        + unoSi False
        + unoSi (tieneM (Criatura Troll) (Just (Criatura Troll)))
        + unoSi True
    =                                                                                               (tieneM.2)
        unoSi ((Criatura Troll) == (Criatura Troll))
        + unoSi ((Criatura Troll) == (Criatura Troll))
        + unoSi False
        + unoSi ((Criatura Troll) == (Criatura Troll))
        + unoSi True
    =                                                                                               (==)
        unoSi True
        + unoSi ((Criatura Troll) == (Criatura Troll))
        + unoSi False
        + unoSi ((Criatura Troll) == (Criatura Troll))
        + unoSi True
    =                                                                                               (==)
        unoSi True
        + unoSi True
        + unoSi False
        + unoSi ((Criatura Troll) == (Criatura Troll))
        + unoSi True
    =                                                                                               (==)
        unoSi True + unoSi True + unoSi False + unoSi True + unoSi True
    =                                                                                               (unoSi.1)
        1 + unoSi True + unoSi False + unoSi True + unoSi True
    =                                                                                               (unoSi.1)
        1 + 1 + unoSi False + unoSi True + unoSi True
    =                                                                                               (unoSi.1)
        1 + 1 + unoSi False + 1 + unoSi True
    =                                                                                               (unoSi.1)
        1 + 1 + unoSi False + 1 + 1
    =                                                                                               (unoSi.1)
        1 + 1 + 0 + 1 + 1
    =                                                                                               (aritmética)
        4

    -- LADO DERECHO

        4

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 5.E


