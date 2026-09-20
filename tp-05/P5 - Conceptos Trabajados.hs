--------------------------------------------------------------------------------------------------------

--                               TRABAJO PRÁCTICO N°5: Tipos Algebraicos                              --
--                                         Conceptos Trabajados                                       --

--------------------------------------------------------------------------------------------------------

> Los elementos de los Tipos Algebraicos no tienen redexes:
    > 

> Los constructores pueden ser funciones:
    > Los constructores pueden ser funciones, porque algunas en su definición están aplicadas y esperan parámetros.
        - Ejemplo:

            Vasito :: Gusto -> Helado
            Cucurucho :: Gusto -> Gusto -> Helado
            Pote :: Gusto -> Gusto -> Gusto -> Helado


> Las Funciones pueden dar significado:
    > Dado que una función es una regla de correspondencia de un conjunto de entrada A, a un conjunto de salida B;
      puede ser interesante que una función le aporte significado a un Tipo Algebraico.
    > Entonces, resulta útil tener funciones que dado un Tipo Algebraico pueda brindarle un significado según el elemento dado.
        - Ejemplo:

            dbAsBool :: DigBin -> Bool
            dbAsBool I = True
            dbAsBool O = False

            dbAsInt :: DigBin -> Int
            dbAsInt I = 1
            dbAsInt O = 0


> El tipo como documentación rudimentaria:
    > El tipo da la suficiente información para tener una buena idea de la implementación de una función.
    > La razón de esto es porque restringe el dominio de las posibles soluciones al funcionar como un contrato a cumplir.
        - Ejemplo:

            tryCatch :: MayFail a -> (a -> b) -> ExHandler b ->
            tryCatch (Raise e) f fe = fe e
            tryCatch (Ok x)    f _  = f x