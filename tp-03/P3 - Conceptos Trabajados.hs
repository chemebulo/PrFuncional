--------------------------------------------------------------------------------------------------------

--                                 TRABAJO PRÁCTICO N°3: Currificación                                --
--                                         Conceptos Trabajados                                       --

--------------------------------------------------------------------------------------------------------

> Funciones currificadas y no currificadas:

      > Funciones currificadas:
            - Aquellas que toman un argumento a la vez, retornando una función intermedia que completa el trabajo.
                  - Ejemplo:
                        suma :: Int -> Int -> Int
                        suma n m = n + m

                        const :: a -> b -> a
                        const x y = x

      > Funciones no currificadas:
            - Aquellas que toman una tupla como argumento, donde cada componente de la misma son "los argumentos".
                  - Ejemplo:
                        suma' :: (Int, Int) -> Int
                        suma' (n, m) = n + m

                        fst :: (a, b) -> a
                        fst (x, y) = x


> Forma de asociación en funciones y tipo:

      > La idea de la asociación (junto con el orden de procedencia) es poder escribir expresiones complejas
        con la menor cantidad de paréntesis posibles conservando el significado original.
      > Asociar bien, suele mejorar la legibilidad del código.
      > Asociar mal, suele hacer que una expresión compleja signifique algo diferente o mismo reescribirla sin significado.

      > Forma de asociación en funciones:
            - Las funciones asocian a izquierda.
                  - Ejemplo de uso correcto:

                        ((compose twice) suma) 2         |     compose twice suma 2
                        ((curry (\p -> fst p)) 3) 2      |     curry (\p -> fst p) 3 2

                  - Ejemplo de uso incorrecto:

                        uncurry (curry snd)              |     uncurry curry snd
                        compose (compose doble doble)    |     compose compose doble doble

      > Forma de asociación en el tipo:
            - El tipo de una función asocia a derecha.
                  - Ejemplo de uso correcto:

                        (((a -> (b -> c)) -> a) -> b) -> c    |     (a -> b -> c) -> a -> b -> c
                        (((a, a) -> b) -> a) -> b             |     ((a, a) -> b) -> a -> b

                  - Ejemplo de uso incorrecto:

                        (Int -> Int) -> Int -> Int            |     Int -> Int -> Int -> Int
                        (a -> b -> c) -> (a -> b) -> a -> c   |     (a -> b -> c) -> a -> b -> a -> c 


> Pasaje de parámetros de un lado a otro:

      > Lado derecho a izquierdo:
            - Ejemplo:
                  cuadruple x = compose doble doble x         |     cuadruple = (\x -> compose doble doble x)
                  fourTimes f x = f (f (f (f x)))             |     fourTimes = (\f x -> f (f (f (f x))))

      > Lado izquierdo a derecho: 
            - Ejemplo:
                  cuadruple = (\x -> compose doble doble x)   |    cuadruple x = compose doble doble x
                  fourTimes = (\f x -> f (f (f (f x))))       |    fourTimes f x = f (f (f (f x)))
    

> Sección de operadores:

      > Es una sintaxis especial para aplicación parcial en operadores infijos.
      > La notación infijo es aquella donde el nombre se encuentra entre sus argumentos.
      > La notación prefijo es aquella donde el nombre se encuentra antes de sus argumentos.

      > Al poner entre paréntesis el operador y uno de sus argumentos devuelve una función
        intermedia que espera el segundo argumento.

      > Sección Izquierda:
            Infijo:  (2*)
            Prefijo: (*) 2

      > Sección Derecha:
            Infijo:  (*2)
            Prefijo: flip (*) 2

      > Backticks:
            Infijo:  foldr (+) 0
            Prefijo: (+) `foldr` 0