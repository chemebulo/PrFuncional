--------------------------------------------------------------------------------------------------------

--                              TRABAJO PRÁCTICO N°12: Esquemas de Funciones II                       --

--------------------------------------------------------------------------------------------------------

> Ejercicio 1:

data ExpA = Cte Int
          | Suma ExpA ExpA
          | Prod ExpA Exp
    deriving Show


-- 1.A

foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA f g h (Cte n)      = f n
foldExpA f g h (Suma e1 e2) = g (foldExpA f g h e1) (foldExpA f g h e2)
foldExpA f g h (Prod e1 e2) = h (foldExpA f g h e1) (foldExpA f g h e2)

-- 1.B.I

cantidadDeCeros :: ExpA -> Int
cantidadDeCeros = foldExpA unoSiCumple (0==) (+) (+)

unoSiCumple :: (a -> Bool) -> a -> Int
unoSiCumple f x = if f x then 1 else 0

-- 1.B.II

noTieneNegativosExplicitosExpA :: ExpA -> Bool
noTieneNegativosExplicitosExpA = foldExpA (flip (>=) 0) (&&) (&&)

-- 1.B.III

simplificarExpA' :: ExpA -> ExpA
simplificarExpA' = foldExpA (\n -> Cte n) simplificarSuma simplificarProd

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma e1       (Cte 0) = e1
simplificarSuma (Cte  0) e2      = e2
simplificarSuma e1       e2      = Suma e1 e2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd _        (Cte 0) = Cte 0
simplificarProd (Cte  0) _       = Cte 0
simplificarProd e1       (Cte 1) = e1
simplificarProd (Cte  1) e2      = e2
simplificarProd e1       e2      = Prod e1 e2

-- 1.B.IV

evalExpA' :: ExpA -> Int
evalExpA' = foldExpA id (+) (*)

-- 1.B.V

showExpA :: ExpA -> String
showExpA = foldExpA (\c -> "(Cte " ++ (c:[]) ++ ")") (\s1 s2 -> "(Suma " ++ s1 ++ s2 ++ ")") (\s1 s2 -> "(Prod " ++ s1 ++ s2 ++ ")")

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