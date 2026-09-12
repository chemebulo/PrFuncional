--------------------------------------------------------------------------------------------------------

--                          TRABAJO PRÁCTICO N°1: Modelo de Cómputo Funcional                         --
--                                         Conceptos Trabajados                                       --

--------------------------------------------------------------------------------------------------------

> Distintas formas de escribir funciones:

    -- Funciones nombradas:

        sumarDos n = n + 2

        twice f x = f (f x)

    -- Funciones anónimas:

        (\ n -> n + 1)

        (\ f -> \ x -> f (f x))

    -- Funciones de alto orden:

        twice doble

        compose (twice doble) cuadruple


> Mecanismo de reducción:

        ((twice twice) doble) 3
    ->                                                      (def. twice, f <- twice)
        (g doble) 3                                             (donde g x = twice (twice x))
    ->                                                      (def. g, x <- doble)
        (twice (twice doble)) 3
    ->                                                      (def. twice, f <- twice doble)
        g' 3                                                    (donde g' x = (twice doble) ((twice doble) x))
    ->                                                      (def. g', x <- 3)
        (twice doble) ((twice doble) 3) 
    ->                                                      (def. twice, f <- doble)
        g'' ((twice doble) 3)                                   (donde g'' x = doble (doble x))
    ->                                                      (def. g'', x <- ((twice doble) 3))
        doble (doble ((twice doble) 3))
    ->                                                      (def. twice, f <- doble)
        doble (doble (g''' 3))                                  (donde g''' x = doble (doble x))
    ->                                                      (def. g''', x <- 3)
        doble (doble (doble (doble 3)))
    ->                                                      (def. doble, x <- 3)
        doble (doble (doble (3 + 3)))
    ->                                                      (def. doble, x <- (3 + 3))
        doble (doble ((3 + 3) + (3 + 3)))
    ->                                                      (aritmética)
        doble (doble 12)
    ->                                                      (def. doble, x <- 12)
        doble (12 + 12)
    ->                                                      (def. doble, x <- (12 + 12))
        (12 + 12) + (12 + 12)
    ->                                                      (aritmética)
        48


> Diferentes ordenes de Reducción:

    -- Orden Normal:

        > Se elige el redex más externo de todos.

        -- Ejemplo:

            doble (doble 12)
        ->                              (def. doble, x <- doble 12)
            (doble 12) + (doble 12)

    -- Orden Aplicativo:

        > Se elige el redex más interno de todos.

        -- Ejemplo:

            doble (doble 12)
        ->                              (def. doble, x <- 12)
            (doble 24)


> Utilidad del "let", "if-then-else", "case", "where":

    -- Let:

        >
        >

        -- Ejemplo:

            (\n -> if n > 0 then Chocolate else Frutilla) 

    -- If-then-else:

        > Es una expresión condicional, la cual permite elegir entre dos alternativas según una condición booleana.
        > Es obligatorio escribir la rama del else, ya que toda la estructura debe devolver un valor del tipo que retorna.

        -- Ejemplo:

            (\n -> if n > 0 then Chocolate else Frutilla) 

    -- Case:

        >
        >

        -- Ejemplo:

            (\n -> if n > 0 then Chocolate else Frutilla) 

    -- Where:

        >
        >

        -- Ejemplo:

            (\n -> if n > 0 then Chocolate else Frutilla) 