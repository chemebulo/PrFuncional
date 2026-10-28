--------------------------------------------------------------------------------------------------------

--                              TRABAJO PRÁCTICO N°12: Esquemas de Funciones II                       --

--------------------------------------------------------------------------------------------------------

> Ejercicio 1:

data ExpA = Cte Int
          | Suma ExpA ExpA
          | Prod ExpA Exp
    deriving Show


-- 1.A

foldExpA :: ... -> ... -> ... -> ... -> ...
foldExpA
foldExpA
foldExpA

-- 1.B.I

cantidadDeCeros :: ExpA -> Int
cantidadDeCeros =

-- 1.B.II

noTieneNegativosExplicitosExpA :: ExpA -> Bool
noTieneNegativosExplicitosExpA =

-- 1.B.III

simplificarExpA' :: ExpA -> ExpA
simplificarExpA' =

-- 1.B.IV

evalExpA' :: ExpA -> Int
evalExpA' =

-- 1.B.V

showExpA :: ExpA -> String
showExpA =

-- 1.C

¿evalExpA' = evalExpA?

-- 1.D

recExpA :: ... -> ... -> ... -> ... -> ... -> ...

-- 1.E.I

cantDeSumaCeros :: ExpA -> Int
cantDeSumaCeros =

-- 1.E.II

cantDeProdUnos :: ExpA -> Int
cantDeProdUnos =