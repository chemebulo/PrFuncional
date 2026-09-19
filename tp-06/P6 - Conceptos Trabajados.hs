--------------------------------------------------------------------------------------------------------

--                         TRABAJO PRÁCTICO N°6: Propiedades y Demostraciones                         --
--                                         Conceptos Trabajados                                       --

--------------------------------------------------------------------------------------------------------

> Conceptos detrás de las propiedades:

    > La identidad en funciones que toman dos parámetros:
        - Para todo f. para todo x. para todo y. curry (uncurry f) x y = f x y
        - Para todo f. para todo x. para todo y. (curry . uncurry) f x y = id f x y

    > La identidad en funciones que toman un par como parámetro:
        - Para todo f. Para todo p. uncurry (curry f) p = f p

    > La identidad en funciones que toman tres parámetros:
        - Para todo f. para todo p. appAssoc (uncurry (uncurry f)) p = uncurry (compose uncurry f) p

    > La identidad para pares:
        - Para todo p. swap (swap p) = id p

    > El operador de composición tiene su equivalente como función nombrada:
        - Para todo f. para todo g. para todo x. (f . g) x = compose f g x

    > La composición es asociativa:
        - Para todo f. para todo g. para todo h. para todo x. (f . (g . h)) x = ((f . g) . h) x

    > Aplicar el cambio de asociación en pares es lo mismo que componer ambas funciones:
        - Para todo p. appAssoc f p = (f . assoc) p
        - Para todo p. appAssoc f p = f (assoc p)


> Distintas formas de expresar situaciones en los planteos de las demostraciones:

    > Estructuras de demostraciones con principio de extensionalidad:
        > Versión 1 (Básica):
            - Por principio de extensionalidad, es equivalente demostrar que:
              ¿...?

              Sea ... Se verá que ¿...?


        > Versión 2 (Aplicando definiciones):
            - Por principio de extensionalidad, es equivalente demostrar que:
              ¿...?

              Por definición de ... es equivalente demostrar que:
              ¿...?

              Sea ... Se verá que ¿...?


        > Versión 3 (Aplicando definiciones y varias veces el principio):
            - Por principio de extensionalidad (nro_veces), es equivalente demostrar que:
              ¿...?

              Por definición de ... es equivalente demostrar que:
              ¿...?

              Sea ... Se verá que ¿...?


    > Expresiones válidas dentro del "Sea ...":
        > Si únicamente hay una variable:
            - "Sea x :: a. Se verá que ¿f x = g x?"

        > Si hay más de dos variables:
            - "Sea x :: a, sea y :: b, ... Se verá que ¿f x y = g x y?"

        > Formas de expresar los distintos tipos de variables:
            > Pares:
                - "Sea (x, y) un par de tipo (a, b). Se verá que ¿f (x, y) = g (x, y)?"
                - "Sea (x, y) :: (a, b). Se verá que ¿f (x, y) = g (x, y)?"
                
                - "Sea p un par de tipo (a, b). Se verá que ¿f p = g p?"
                - "Sea p :: (a, b). Se verá que ¿f p = g p?"


            > Funciones:
                - "Sea h :: (a -> b). Se verá que ¿h x = g x?"
                - "Sea h una función de tipo (a -> b). Se verá que ¿h x = g x?"
                - "Sea h una función cualquiera. Se verá que ¿h x = g x?"


            > Elementos de tipos polimórficos:
                - "Sea x :: a. Se verá que ¿f x = g y?"
                - "Sea x :: a, sea y :: b. Se verá que ¿f x = g y?"


            > Elementos con tipos instanciados:
                - "Sea x un número. Se verá que ¿f x = g y?"
                - "Sea x e y dos números. Se verá que ¿f x = g y?"
                - "Sea b un booleano. Se verá que ¿f x = g y?"

        - La idea es ser consistente con estos usos, aunque cualquiera de las formas son válidas dentro de su contexto.