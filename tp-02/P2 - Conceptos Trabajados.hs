--------------------------------------------------------------------------------------------------------

--                               TRABAJO PRÁCTICO N°2: Sistemas de Tipos                              --
--                                         Conceptos Trabajados                                       --

--------------------------------------------------------------------------------------------------------

> Conceptos del sistema de tipo Hindley-Milner Polimórfico:

    > Expresiones válidas:
        Ejemplo:

            2 + 2

            (\n -> n * 2) 4

    > Expresiones inválidas:

        > Expresiones con partes faltantes:
            Ejemplo:

                (2 + 

                let (x, y) = (3, 2) x + y 

        > Expresiones que no tienen sentido:
            Ejemplo:

                div 1 0

                a

        > Expresiones que podrían tener sentido:
            Ejemplo:

                if True then 1 else False

                2 + False

    > Polimorfismo paramétrico:

        > Es una característica del sistema de tipos, que permite expresar que un cierto parámetro puede
          admitir infinitos tipos, asignando más general que pueda transformarse en cualquiera de ellos.
            Ejemplo:

            twice :: (a -> a) -> a -> a


> Regla para tipar funciones:

    f :: A -> B             e  :: Int           
    e :: A                  e' :: Int
    -----------             -------------
    f e :: B                e + e' :: Int


    f :: A -> B -> C        (.) :: A -> B -> C
    g :: A                  f   :: A
    h :: B                  g   :: B
    ----------------        ------------------
    f g h :: C              f . g :: C

    Ejemplo:

        doble   :: Int -> Int
        doble 2 :: Int
        ----------------------
        doble (doble 2) :: Int

        doble   :: Int -> Int
        2       :: Int
        ----------------------
        doble 2 :: Int

> El tipo como documentación rudimentaria:

    > El tipo da la suficiente información para tener una buena idea de la implementación de una función.
    > La razón de esto es porque restringe el dominio de las posibles soluciones al funcionar como un contrato a cumplir.
    > Puede ser una característica muy útil hoy en día a la hora de desarrollar con IA.