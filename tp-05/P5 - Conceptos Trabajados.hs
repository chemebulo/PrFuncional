--------------------------------------------------------------------------------------------------------

--                               TRABAJO PRÁCTICO N°5: Tipos Algebraicos                              --
--                                         Conceptos Trabajados                                       --

--------------------------------------------------------------------------------------------------------

> Los constructores pueden ser funciones:
    > 
        - Ejemplo:

            Vasito :: Gusto -> Helado
            Cucurucho :: Gusto -> Gusto -> Helado
            Pote :: Gusto -> Gusto -> Gusto -> Helado

> Funciones para dar significado:



> El tipo como documentación rudimentaria:

    > El tipo da la suficiente información para tener una buena idea de la implementación de una función.
    > La razón de esto es porque restringe el dominio de las posibles soluciones al funcionar como un contrato a cumplir.
        - Ejemplo:
        
            tryCatch :: MayFail a -> (a -> b) -> ExHandler b ->
            tryCatch (Raise e) f fe = fe e
            tryCatch (Ok x)    f _  = f x


> Tipos algebraicos por comprensión: