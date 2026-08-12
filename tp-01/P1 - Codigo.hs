--------------------------------------------------------------------------------------------------------

--                          TRABAJO PRÁCTICO N°1: Modelo de Cómputo Funcional                         --

--------------------------------------------------------------------------------------------------------

> Ejercicio 1:

--  10 - 6
--  2 * 2
--  doble 2
--  doble (doble 1)
--  doble 1 + doble 1
--  (\n -> (+) 2 2) 99
--  (\n -> (4*) 1) 99
--  cuadruple 1


> Ejercicio 2:

    doble (doble 2)
->                          (def. doble, x <- doble 2)
    (doble 2) + (doble 2)
->                          (def. doble, x <- 2)
    2 + 2 + (doble 2)
->                          (def doble, x <- 2)
    2 + 2 + 2 + 2
->                          (aritmética)
    8


> Ejercicio 3:

-- 3.A:

    cuadruple 2
->                          (def. cuadruple, x <- 2)
    4 * 2
->                          (aritmética)
    8

-- 3.B:

    cuadruple (cuadruple 2)
->                          (def. cuadruple, x <- cuadruple 2)
    4 * (cuadruple 2)
->                          (def. cuadruple, x <- 2)
    4 * (4 * 2)
->                          (aritmética)
    4 * 8
->                          (aritmética)
    32


> Ejercicio 4:

triple :: Int -> Int
triple n = n * 3

succ :: Int -> Int
succ n = n + 1

sumarDos :: Int -> Int
sumarDos n = n + 2


> Ejercicio 5:

twice succ = sumarDos

-- LADO IZQUIERDO:

    (twice succ) 2
->                      (def. twice, f <- succ)
    g 2
->                      (def. twice, x <- 2)
    succ (succ 2)
->                      (def. succ, x <- succ 2)
    (succ 2) + 1
->                      (def. succ, x <- 2)
    (2 + 1) + 1
->                      (aritmética)
    4

-- LADO DERECHO:

    sumarDos 2
->                      (def. sumarDos, n <- 2)
    2 + 2
->                      (aritmética)
    4

-- Ambas funciones transforman el mismo argumento de la misma forma, se puede afirmar que son denotacionalmente equivalentes.


> Ejercicio 6:

-- doble = (\n -> n * 2)

-- sumarDos = twice succ 

-- twice (2*) = cuadruple 


> Ejercicio 7:

    ((twice twice) doble) 3
->                                  (def. twice, f <- twice)
    ...-



> Ejercicio 8:

-- 8.A: 

triple = (\n -> n * 3)

-- 8.B: 

succ = (\n -> n + 1)

-- 8.C: 

sumarDos = (\n -> n + 2)

-- 8.D: 

twice = (\f -> (\ x -> f (f x)))

-- 8.E: 

twice twice = (\f -> (\x -> f (f (f (f x)))))


> Ejercicio 9:

-- 9.A:

f x = let (y, z) = (x, x)
      in y

-- 9.B: 

f (x, y) = let z = x + y 
            in g (z, y)
    where g (a,b) = a - b

-- 9.C:

f p = case p of (x,y)
        -> x


-- 9.D:

f = \ p -> let (x, y) = p 
           in y