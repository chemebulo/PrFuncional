--------------------------------------------------------------------------------------------------------

--                         TRABAJO PRÁCTICO N°6: Propiedades y Demostraciones                         --

--------------------------------------------------------------------------------------------------------

> Ejercicio 1:

-- 1.A

¿doble = \x -> 2 * x?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿para todo m. doble m = (\x -> 2 * x) m?
    
    Sea n un número cualquiera. Se verá que ¿doble n = (\x -> 2 * x) n?

    -- LADO IZQUIERDO:

        doble n
    =                       (doble, x <- n)
        n + n

    -- LADO DERECHO:

        (\x -> 2 * x) n
    =                       (regla beta, x <- n)
        2 * n
    =                       (aritmética)
        2 + 2

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 1.B

¿compose doble doble = cuadruple?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que
    ¿para todo m. compose doble doble m = cuadruple m?

    Sea n un número cualquiera. Se verá que ¿compose doble doble n = cuadruple n?

    -- LADO IZQUIERDO:

        compose doble doble n
    =                               (compose, f <- doble, g <- doble, x <- n)
        doble (doble n)
    =                               (doble, x <- doble n)
        (doble n) + (doble n)
    =                               (doble, x <- n)
        (n + n) + (doble n) 
    =                               (doble, x <- n)
        (n + n) + (n + n) 

    -- LADO DERECHO:

        cuadruple n
    =                               (cuadruple, x <- n)
        4 * n
    =                               (aritmética)
        (n + n) + (n + n) 

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


> Ejercicio 2:

-- 2.A

¿para todo x. para todo y. x && y = not ((not x) || (not y))?

Demostración:
    Sea b1 :: Bool, sea b2 :: Bool. Se verá que ¿b1 && b2 = not ((not b1) || (not b2))?

    Caso b1 = True:

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

    Caso b1 = False:

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

    Caso b1 = ⊥:

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
        Sea b :: Bool. Se verá que ¿not (not b) = b?

        Caso b = True:

        -- LADO IZQUIERDO:

            not (not True)
        =                                       (not.1)
            not False
        =                                       (not.2)
            True

        -- LADO DERECHO: 

            True

        -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso b = False:

        -- LADO IZQUIERDO:

            not (not False)
        =                                       (not.2)
            not True
        =                                       (not.1)
            False

        -- LADO DERECHO: 

            False

        -- Ambos lados llegan a lo mismo, el caso es válido.

        Caso b = ⊥:

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
    Sea b1 :: Bool, sea b2 :: Bool. Se verá que ¿not (b1 || b2) = not b1 && not b2?

    Caso b1 = True:

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

    Caso b1 = False:

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

    Caso b1 = ⊥:

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

Demostración:
    Por principio de extensionalidad (dos veces), es equivalente demostrar que:
    ¿Para todo x. para todo y. curry suma' x y = suma x y?

    Sea n :: Int, sea m :: Int. Se verá que ¿curry suma' n m = suma n m?

    -- LADO IZQUIERDO:

        curry suma' n m
    =                               (curry, f <- suma', x <- n, y <- m)
        suma' (n, m)
    =                               (suma', (x, y) <- (n, m))
        n + m

    -- LADO DERECHO:

        suma n m
    =                               (suma, x <- n, y <- m)
        n + m

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 3.B

¿uncurry suma = suma'?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que:
    ¿Para todo p. uncurry suma p = suma' p?

    Sea (n, m) un par de tipo (Int, Int). Se verá que ¿uncurry suma (n, m) = suma' (n, m)?

    -- LADO IZQUIERDO:

        uncurry suma (n, m)
    =                           (uncurry, f <- suma, p <- p)
        suma n m          
    =                           (suma, x <- n, y <- m)
        n + m

    -- LADO DERECHO:

        suma' (n, m)
    =                           (suma', (x, y) <- (n, m))
        n + m

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


> Ejercicio 4:

-- 4.A

¿curry fst = const?

Demostración:
    Por principio de extensionalidad (dos veces), es equivalente demostrar que:
    ¿Para todo x. para todo y. curry fst x y = const x y?

    Sea w :: a, sea z :: b. Se verá que ¿curry fst w z = const w z?

    -- LADO IZQUIERDO:

        curry fst w z
    =                           (curry, f <- fst, x <- w, y <- z)
        fst (w, z)
    =                           (fst, (x, y) <- (w, z))
        w

    -- LADO DERECHO:

        const w z
    =                           (const, x <- w, y <- z)
        w

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 4.B

¿uncurry (flip const) = snd?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que:
    ¿Para todo p. uncurry (flip const) p = snd p?

    Sea (w, z) un par de tipo (a, b). Se verá que ¿uncurry (flip const) (w, z) = snd (w, z)?

    -- LADO IZQUIERDO:

        uncurry (flip const) (w, z)
    =                                       (uncurry, f <- flip const, (x, y) <- (w, z))
        flip const w z
    =                                       (flip, f <- const, x <- w, y <- z)
        const z w
    =                                       (const, x <- z, y <- w)
        z

    -- LADO DERECHO:

        snd (w, z)
    =                                       (snd, (x, y) <- (w, z))
        z

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


> Ejercicio 5:

-- 5.A

¿para todo f. curry (uncurry f) = f?

Demostración:
    Por principio de extensionalidad (dos veces), es equivalente demostrar que:
    ¿Para todo f. para todo x. para todo y. curry (uncurry f) x y = f x y

    Sea g una función de tipo (a -> b -> c), sea w :: a, sea z :: b.
    Se verá que ¿curry (uncurry g) w z = g w z?

    -- LADO IZQUIERDO:

        curry (uncurry g) w z
    =                                   (curry, f <- uncurry g, x <- w, y <- z)
        uncurry g (w, z)
    =                                   (uncurry, f <- g, x <- w, y <- z)
        g w z

    -- LADO DERECHO:

        g w z

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 5.B

¿para todo f. uncurry (curry f) = f?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que:
    ¿Para todo f. Para todo p. uncurry (curry f) p = f p?

    Sea (w, z) un par de tipo (a, b), sea g una función de tipo ((a, b) -> c).
    Se verá que ¿uncurry (curry g) (w, z) = g (w, z)?

    -- LADO IZQUIERDO:

        uncurry (curry g) (w, z)
    =                                       (uncurry, f <- curry g, (x, y) <- (w, z))
        curry g w z
    =                                       (curry, f <- g, x <- w, y <- z)
        g (w, z)

    -- LADO DERECHO:

        g (w, z)

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


> Ejercicio 6:

¿para todo f. appAssoc (uncurry (uncurry f)) = uncurry (compose uncurry f)?

Demostración:
    Por principio de extensionalidad, es equivalente demostrar que:
    ¿Para todo f. para todo p. appAssoc (uncurry (uncurry f)) p = uncurry (compose uncurry f) p?

    Sea g una función de tipo (a -> b -> c -> d), sea (x, (y, z)) :: (a, (b, c)).
    Se verá que ¿appAssoc (uncurry (uncurry g)) (x, (y, z)) = uncurry (compose uncurry g) (x, (y, z))? 

    -- LADO IZQUIERDO:

        appAssoc (uncurry (uncurry g)) (x, (y, z))
    =                                                   (appAssoc, f <- uncurry (uncurry g), p <- (x, (y, z)))
        uncurry (uncurry g) (assoc (x, (y, z)))
    =                                                   (assoc, (x, (y, z)) <- (x, (y, z)))
        uncurry (uncurry g) ((x, y), z)
    =                                                   (uncurry, f <- uncurry g, (x, y) <- ((x, y), z))
        uncurry g (x, y) z
    =                                                   (uncurry, f <- g, (x, y) <- (x, y))
        g x y z                                         

    -- LADO DERECHO:

        uncurry (compose uncurry g) (x, (y, z))
    =                                                   (uncurry, f <- compose uncurry g, (x, y) <- (x, (y, z)))
        compose uncurry g x (y, z)
    =                                                   (compose, f <- uncurry, g <- g, x <- x)
        uncurry (g x) (y, z)
    =                                                   (uncurry, f <- g x, (x, y) <- (y, z))
        g x y z

    -- Ambos lados llegan a lo mismo, la propiedad es válida.


> Ejercicio 7:

-- 7.A

    -- 7.A.I.

    cuadruple = doble . doble

    -- 7.A.II.

    doble = (*2) . id

    -- 7.A.III.

    twice = uncurry (.) . dup

    -- 7.A.IV.

    many 0 f = id . id
    many n f = f . (many (n-1) f)


-- 7.B.

    -- 7.B.I.

    ¿para todo f. para todo g.  f . g = compose f g?
    
    Demostración:
        Por principio de extensionalidad, es equivalente demostrar que:
        ¿Para todo f. para todo g. para todo x. (f . g) x = compose f g x?

        Por definición de (.), es equivalente demostrar que:
        ¿Para todo f. para todo g. para todo x. f (g x) = compose f g x?

        Sea h una función de tipo (a -> b), sea k una función de tipo (c -> a), sea y :: c.
        Se verá que ¿h (k y) = compose h k y?

        -- LADO IZQUIERDO:

            h (k y)

        -- LADO DERECHO:

            compose h k y
        =                           (compose, f <- h, g <- k, x <- y)
            h (k y)

        -- Ambos lados llegan a lo mismo, la propiedad es válida.


    -- 7.B.II.

    ¿swap . swap = id?

    Demostración:
        Por principio de extensionalidad, es equivalente demostrar que:
        ¿Para todo p. (swap . swap) p = id p?

        Por definición de (.), es equivalente demostrar que:
        ¿Para todo p. swap (swap p) = id p?

        Sea (x, y) un par de tipo (a, b). Se verá que ¿swap (swap (x, y)) = id (x, y)?

        -- LADO IZQUIERDO:

            swap (swap (x, y))
        =                               (swap, (x, y) <- (x, y))
            swap (y, x)
        =                               (swap, (x, y) <- (y, x))
            (x, y)

        -- LADO DERECHO:

            id (x, y)
        =                               (id, (x, y) <- (x, y))
            (x, y)

        -- Ambos lados llegan a lo mismo, la propiedad es válida.


    -- 7.B.III.

    ¿para todo f. para todo g. para todo h. f . (g . h) = (f . g) . h?

    Demostración:
        Por principio de extensionalidad, es equivalente demostrar que:
        ¿Para todo f. para todo g. para todo h. para todo x. (f . (g . h)) x = ((f . g) . h) x?

        Sea t, s, k funciones cualquiera, sea y :: c. Se verá que ¿(t . (s . k)) y = ((t . s) . k) y?

        -- LADO IZQUIERDO:

            (t . (s . k)) y
        =                               ((.), f <- t, g <- (s. k), x <- y)
            t ((s . k) y)
        =                               ((.), f <- s, g <- k, x <- y)
            t (s (k y))

        -- LADO DERECHO:

            ((t . s) . k) y
        =                               ((.), f <- (t . s), g <- k, x <- y)
            (t . s) (k y)
        =                               ((.), f <- t, g <- s, x <- (k y))
            t (s (k y))

        -- Ambos lados llegan a lo mismo, la propiedad es válida.


    -- 7.B.IV.

    ¿curry . uncurry = id?

    Demostración:
        Por principio de extensionalidad (tres veces), es equivalente demostrar que:
        ¿Para todo f. para todo x. para todo y. (curry . uncurry) f x y = id f x y?

        Por definición de (.), es equivalente demostrar que:
        ¿Para todo f. para todo x. para todo y. curry (uncurry f) x y = id f x y?

        Sea g una función de tipo (a -> b -> c), sea w :: a, sea z :: b. Se verá que ¿curry (uncurry g) w z = id g w z?

        -- LADO IZQUIERDO:

            curry (uncurry g) w z
        =                                   (curry, f <- (uncurry g), x <- w, y <- z)
            uncurry g (w, z)
        =                                   (uncurry, f <- g, (x, y) <- (w, z))
            g w z

        -- LADO DERECHO: 

            id g w z
        =                                   (id, x <- g)
            g w z

        -- Ambos lados llegan a lo mismo, la propiedad es válida.


    -- 7.B.V.

    ¿para todo f. appAssoc f = f . assoc?

    Demostración:
        Por principio de extensionalidad, es equivalente demostrar que:
        ¿Para todo p. appAssoc f p = (f . assoc) p?

        Por definición de (.), es equivalente demostrar que:
        ¿Para todo p. appAssoc f p = f (assoc p)?

        Sea p' un par de tipo (a, (b, c)). Se verá que ¿appAssoc f p' = f (assoc p')?

        -- LADO IZQUIERDO:

            appAssoc f p'
        =                                       (appAssoc, f <- f, p <- p')
            f (assoc p')

        -- LADO DERECHO:

            (f . assoc) p'
        =                                       ((.), f <- f, g <- assoc, x <- p')
            f (assoc p')

        -- Ambos lados llegan a lo mismo, la propiedad es válida.


-- 7.C.

    -- 7.C.I.

    ¿doble . doble = cuadruple?

    Demostración:

            (doble . doble)
        =                                   (Ejercicio 7.B.I)
            compose doble doble
        =                                   (Ejercicio 1.B)
            cuadruple

    -- Mediante otras propiedades, queda demostrado entonces que la propiedad es válida.
    

    -- 7.C.II.

    ¿para todo f'. curry (uncurry (curry f')) = curry f'?

    Demostración:

        curry (uncurry (curry f'))
    =                                       (Ejercicio 5.B)
        curry f'

    -- Mediante otras propiedades, queda demostrado entonces que la propiedad es válida.


    -- 7.C.III.

    ¿para todo f. appAssoc (uncurry (uncurry f)) = (uncurry . uncurry) f . assoc?

    Demostración:

        appAssoc (uncurry (uncurry f))
    =                                       (Ejercicio 7.B.V)
        (uncurry (uncurry f)) . assoc
    =                                       ((.))
        (uncurry . uncurry) f . assoc

    -- Mediante otras propiedades, queda demostrado entonces que la propiedad es válida.


    -- 7.C.IV.

    ¿para todo f. (uncurry . uncurry) f . assoc = uncurry (uncurry . f)?

    Demostración:

        (uncurry . uncurry) f . assoc
    =                                       ((.))
        (uncurry (uncurry f)) . assoc
    =                                       (Ejercicio 7.B.V)
        appAssoc (uncurry (uncurry f))
    =                                       (Ejercicio 6)
        uncurry (compose uncurry f)
    =                                       ((.))
        uncurry (uncurry . f)

    -- Mediante otras propiedades, queda demostrado entonces que la propiedad es válida.