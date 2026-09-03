--------------------------------------------------------------------------------------------------------

--                                   TRABAJO PRÁCTICO N°4: Reducción                                  --

--------------------------------------------------------------------------------------------------------

> Ejercicio 1:

-- 1.A

-- La función es parcial, ya que udiv denota bottom al recibir (3, 0) que es bien definido.
udiv :: (Int, Int) -> Int
udiv (x, y) = div x y


-- 1.B

-- La función es parcial, ya que udivE denota bottom al recibir (3, 0) que es bien definido.
udivE :: (Int, Int) -> Int
udivE (x, 0) = error "No puedo dividir por 0" 
udivE (x, y) = div x y


-- 1.C

-- La función es parcial, ya que udivH denota bottom al recibir (3, 0) que es bien definido.
udivH :: (Int, Int) -> Int
udivH = uncurry div


-- 1.D

-- La función es total, ya que succ no tiene forma de recibir algo bien definido que al aplicarlo denote bottom.
succ :: Int -> Int
succ x = x + 1


-- 1.E

-- La función es total, ya que succH no tiene forma de recibir algo bien definido que al aplicarlo denote bottom.
succH :: Int -> Int
succH = suma 1 


-- 1.F

-- La función es total, ya que porLaMitad no tiene forma de recibir algo bien definido que al aplicarlo denote bottom.
porLaMitad :: Int -> Int
porLaMitad = flip div 2


-- 1.G

-- La función es parcial, ya que conDieresis denota bottom al recibir 'e' que es bien definido.
conDieresis 'u' = 'ü'


-- 1.H

-- La función es parcial, ya que conDieresisB denota bottom al recibir 'e' que es bien definido.
conDieresisB 'u' = 'ü' 
conDieresisB c   = conDieresisB c 


-- 1.I

-- La función es parcial, ya que conTildePM denota bottom al recibir 'A' que es bien definido.
conTildePM 'a' = 'á' 
conTildePM 'e' = 'é' 
conTildePM 'i' = 'í' 
conTildePM 'o' = 'ó' 
conTildePM 'u' = 'ú'


-- 1.J

-- La función es parcial, ya que conTildeE denota bottom al recibir 'A' que es bien definido.
conTildeE c = if esVocal c 
                 then conTildePM c 
                 else error "El valor recibido no es vocal"


-- 1.K

-- La función es total, ya que conTilde no tiene forma de recibir algo bien definido que al aplicarlo denote bottom.
conTilde c = if esVocal c && esMinuscula c 
                then conTildePM c 
                else c


> Ejercicio 2:

- Según lo realizado en el ejercicio anterior, estas son las equivalencias entre las funciones:
    - udiv  = udivE
    - udiv  = udivH
    - udivE = udivH
    - succ = succH
    - conDieresis = conDieresisB
    - conTildePM = conTildeE


> Ejercicio 3:

-- Definición de twice = \f -> \x -> f (f x).

-- 3.A

-- Según la definición dada, tiene 9 redexes:
    (\f -> \x -> f (f x)) doble
->                                  (Beta, f <- doble)
    (\x -> doble (doble x))
->                                  (Def. doble, x <- doble x)
    (\x -> (doble x) + (doble x))
->                                  (Def. doble, x <- x)
    (\x -> (x + x) + (doble x))
->                                  (Def. doble, x <- x)
    (\x -> (x + x) + (x + x))


-- 3.B

-- Según la definición dada, tiene 11 redexes:
    (\f -> \x -> f (f x)) doble 2
->                                  (Beta, f <- doble, x <- 2)
    doble (doble 2)
->                                  (Def. doble, x <- doble 2)
    (doble 2) + (doble 2)
->                                  (Def. doble, x <- 2)
    (2 + 2) + (doble 2)
->                                  (Def. doble, x <- 2)
    (2 + 2) + (2 + 2)
->                                  (Aritmética)
    4 + 4 
->                                  (Aritmética)
    8


-- 3.C

-- Según la definición dada, no tiene redexes:
    (\f -> \x -> f (f x))


> Ejercicio 4:

-- Definición de twice f = g 
--                 where g x = f (f x)

-- 4.A

-- Según la definición dada, tiene 1 redex:
    twice doble
->                              (Def. twice, f <- doble)
                                    (donde g x = doble (doble x))
    g

-- 4.B

-- Según la definición dada, tiene 11 redexes:
    twice doble 2
->                              (Def. twice, f <- doble)
                                    (donde g x = doble (doble x))
    g 2
->                              (Def. g, x <- 2)
    doble (doble 2)
->                              (Def. doble, x <- doble 2)
    (doble 2) + (doble 2)
->                              (Def. doble, x <- 2)
    (2 + 2) + (doble 2)
->                              (Def. doble, x <- 2)
    (2 + 2) + (2 + 2)
->                              (Aritmética)
    4 + 4
->                              (Aritmética)
    8


-- 4.C

-- Según la definición dada, no tiene redexes:
    twice


> Ejercicio 5:

-- Definición de twice f x = f (f x)

-- 5.A

-- Según la definición dada, no tiene redexes:
    twice doble


-- 5.B

-- Según la definición dada, tiene 10 redexes:
    twice doble 2
->                          (Def. twice, f <- doble, x <- 2)
    doble (doble 2)
->                              (Def. doble, x <- doble 2)
    (doble 2) + (doble 2)
->                              (Def. doble, x <- 2)
    (2 + 2) + (doble 2)
->                              (Def. doble, x <- 2)
    (2 + 2) + (2 + 2)
->                              (Aritmética)
    4 + 4
->                              (Aritmética)
    8


-- 5.C

-- Según la definición dada, no tiene redexes:
    twice


> Ejercicio 6:

-- 6.A



-- 6.B



-- 6.C

