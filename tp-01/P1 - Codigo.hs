--------------------------------------------------------------------------------------------------------

--                          TRABAJO PRÁCTICO N°1: Modelo de Cómputo Funcional                         --

--------------------------------------------------------------------------------------------------------

> Ejercicio 1:

- Expresión 1:  10 - 6
- Expresión 2:  2 * 2
- Expresión 3:  doble 2
- Expresión 4:  doble (doble 1)
- Expresión 5:  doble 1 + doble 1
- Expresión 6:  (\n -> (+) 2 2) 99
- Expresión 7:  (\n -> (4*) 1) 99
- Expresión 8:  cuadruple 1


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

-- 3.A

    cuadruple 2
->                          (def. cuadruple, x <- 2)
    4 * 2
->                          (aritmética)
    8

-- 3.B

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

-- LADO IZQUIERDO

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

-- LADO DERECHO

    sumarDos 2
->                      (def. sumarDos, n <- 2)
    2 + 2
->                      (aritmética)
    4

-- Ambas funciones transforman el mismo argumento de la misma forma, se puede afirmar que son denotacionalmente equivalentes.


> Ejercicio 6:

- Expresión 1: doble = (\n -> n * 2)

- Expresión 2:  

- Expresión 3: 


> Ejercicio 7:

    ((twice twice) doble) 3
->                                  (def. twice, f <- twice)
    ...