--------------------------------------------------------------------------------------------------------

--                                   TRABAJO PRÁCTICO N°4: Reducción                                  --
--                                         Conceptos Trabajados                                       --

--------------------------------------------------------------------------------------------------------

> Forma de convertir de una función de alto orden en una función anónima:

      > Ejemplo: (.) (.) (.)

      Siendo: (f . g) x = f (g x)

            (\f g x y -> (.) (.) (.) f g x y)
      ->                                              (Def. (.), f <- (.), g <- (.), x <- f)
            (\f g x y -> (.) ((.) f) (.) g x y)
      ->                                              (Def. (.), f <- (.) f, g <- g, x <- x)
            (\f g x y -> (.) f (g x) y)
      ->                                              (Def. (.), f <- f, g <- g x, x <- y)
            (\f g x y -> f (g x y))


> Funciones totales y parciales:

      > Funciones totales:
            > Son aquellas que ... 
                  Ejemplo:

                  succ

      > Funciones parciales:
            > Son aquellas que ... 
                  Ejemplo:

                  div


> Funciones estrictas y no estrictas:

      > Funciones estrictas:
            > Son aquellas que ... 
                  Ejemplo:



      > Funciones no estrictas:
            > Son aquellas que ... 
                  Ejemplo:


> Redexes según la definición de la función:

