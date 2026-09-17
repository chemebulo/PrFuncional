--------------------------------------------------------------------------------------------------------

--                         TRABAJO PRÁCTICO N°6: Propiedades y Demostraciones                         --

--------------------------------------------------------------------------------------------------------

> Ejercicio 1:

-- 1.A

¿doble = \x -> 2 * x?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿para todo m. doble m = (\x -> 2 * x) m?
    
    Sea n un número cualquiera. Se verá que: doble n = (\x -> 2 * x) n

    -- LADO IZQUIERDO:

        doble n
    =                       (doble)
        n + n

    -- LADO DERECHO:

        (\x -> 2 * x) n
    =                       (B)
        2 * n
    =                       (Aritmética)
        2 + 2

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 1.B

¿compose doble doble = cuadruple?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿para todo m. compose doble doble m = cuadruple m?

    Sea n un número cualquiera. Se verá que: compose doble doble n = cuadruple n

    -- LADO IZQUIERDO:

        compose doble doble n
    =                               (compose)
        doble (doble n)
    =                               (doble)
        (doble n) + (doble n)
    =                               (doble)
        (n + n) + (doble n) 
    =                               (doble)
        (n + n) + (n + n) 

    -- LADO DERECHO:

        cuadruple n
    =                               (cuadruple)
        4 * n
    =                               (aritmética)
        (n + n) + (n + n) 

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


> Ejercicio 2:

-- 2.A

¿para todo x. para todo y. x && y = not ((not x) || (not y))?


-- 2.B

¿para todo x. para todo y. not (x || y) = not x && not y?


> Ejercicio 3:

-- 3.A

¿curry suma' = suma?


-- 3.B

¿uncurry suma = suma'?


> Ejercicio 4:

-- 4.A

¿curry fst = const?


-- 4.B

¿uncurry (flip const) = snd?


> Ejercicio 5:

-- 5.A

¿para todo f. curry (uncurry f) = f?


-- 5.B

¿para todo f. curry (uncurry f) = f?


> Ejercicio 6:

¿para todo f. appAssoc (uncurry (uncurry f)) = uncurry (compose uncurry f)?


> Ejercicio 7:

-- 7.A

    -- 7.A.I.

    cuadruple

    -- 7.A.II.

    doble

    -- 7.A.III.

    twice

    -- 7.A.IV.

    many


-- 7.B.

    -- 7.B.I.

    ¿para todo f. para todo g.  f . g = compose f g?

    -- 7.B.II.

    ¿swap . swap = id?

    -- 7.B.III.

    ¿para todo f. para todo g. para todo h. f . (g . h) = (f . g) . h?

    -- 7.B.IV.

    ¿curry . uncurry = id?

    -- 7.B.V.

    ¿para todo f. appAssoc f = f . assoc?


-- 7.C.

    -- 7.C.I.

    ¿doble . doble = cuadruple?
    

    -- 7.C.II.

    ¿para todo f'. curry (uncurry (curry f')) = curry f'?


    -- 7.C.III.

    ¿para todo f. appAssoc (uncurry (uncurry f))   = (uncurry . uncurry) f . assoc?


    -- 7.C.IV.

    ¿para todo f. (uncurry . uncurry) f . assoc   = uncurry (uncurry . f)?

    