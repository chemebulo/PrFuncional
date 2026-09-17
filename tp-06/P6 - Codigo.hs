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
    =                       (Regla Beta)
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

Demostración:
    Sea b1 y b2 dos booleanos cualquiera. Se verá que: b1 && b2 = not ((not b1) || (not b2)) 

    Caso 1 (b1 = True):

    -- LADO IZQUIERDO:

        True && b2
    =                                       (&&.2)
        b2

    -- LADO DERECHO: 

        not ((not True) || (not b2))
    =                                       (not.1)
        not (False || (not b2))
    =                                       (||.2)
        not (not b2)
    =                                       (Lema IdBool)
        b2

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Caso 2 (b1 = False):

    -- LADO IZQUIERDO:

        False && b2
    =                                       (&&.1)
        False

    -- LADO DERECHO: 

        not ((not False) || (not b2))
    =                                       (not.2)
        not (True || (not b2))
    =                                       (||.1)
        not True
    =                                       (not.1)
        False

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Caso 3 (b1 = ⊥):

    -- LADO IZQUIERDO:

        ⊥ && b2
    =                                       (&&)
        ⊥

    -- LADO DERECHO: 

        not ((not ⊥) || (not b2))
    =                                       (not)
        not (⊥ || (not b2))
    =                                       (||)
        not ⊥
    =                                       (not)
        ⊥

    -- Ambos lados llegan a lo mismo, la propiedad es válida.
    
    Lema IdBool: ¿para todo x. not (not x) = x?

    Demostración:
        Sea b un booleano cualquiera. Se verá que: not (not b) = b 

        Caso 1 (b = True):

        -- LADO IZQUIERDO:

            not (not True)
        =                                       (not.1)
            not False
        =                                       (not.2)
            True

        -- LADO DERECHO: 

            True

        -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 2 (b = False):

        -- LADO IZQUIERDO:

            not (not False)
        =                                       (not.2)
            not True
        =                                       (not.1)
            False

        -- LADO DERECHO: 

            False

        -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso 3 (b = ⊥):

        -- LADO IZQUIERDO:

            not (not ⊥)
        =                                       (not)
            not ⊥
        =                                       (not)
            ⊥

        -- LADO DERECHO: 

            ⊥

        -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 2.B

¿para todo x. para todo y. not (x || y) = not x && not y?

Demostración:
    Sea b1 y b2 dos booleanos cualquiera. Se verá que: not (b1 || b2) = not b1 && not b2

    Caso 1 (b1 = True):

    -- LADO IZQUIERDO:

        not (True || b2)
    =                               (||.1)
        not True
    =                               (not.1)
        False

    -- LADO DERECHO: 

        not True && not b2
    =                               (not.1)
        False && not b2
    =                               (&&.1)
        False

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Caso 2 (b1 = False):

    -- LADO IZQUIERDO:

        not (False || b2)
    =                               (||.2)
        not b2

    -- LADO DERECHO: 

        not False && not b2
    =                               (not.2)
        True && not b2
    =                               (&&.2)
        not b2

    -- Ambos lados llegan a lo mismo, el caso es válido.

    Caso 3 (b1 = ⊥):

    -- LADO IZQUIERDO:

        not (⊥ || b2)
    =                               (||)
        not ⊥ 
    =                               (not)
        ⊥

    -- LADO DERECHO: 

        not ⊥ && not b2
    =                               (not)
        ⊥ && not b2
    =                               (&)
        ⊥

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


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

    cuadruple = doble . doble

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

    