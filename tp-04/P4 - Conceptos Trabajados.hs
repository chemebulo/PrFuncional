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
            > Son aquellas que nunca dan bottom si recibe valores totalmente definidos.
                  Ejemplo:
                        succ
                        dup

      > Funciones parciales:
            > Son aquellas que pueden dar bottom al recibir valores totalmente definidos. 
                  Ejemplo:
                        div
                        many

> Funciones estrictas y no estrictas:

      > Funciones estrictas:
            > Son aquellas que siempre devuelve bottom si recibe bottom.
                  Ejemplo:
                        uncurry
                        apply

      > Funciones no estrictas:
            > Son aquellas que pueden devolver algo diferente a bottom si recibe bottom.
                  Ejemplo:
                        const
                        dup