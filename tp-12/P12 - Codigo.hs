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
cantidadDeCeros = foldExpA (\n -> unoSi (n == 0))
                           (+)
                           (+)

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

-- 1.B.II

noTieneNegativosExplicitosExpA :: ExpA -> Bool
noTieneNegativosExplicitosExpA = foldExpA (\n -> n >= 0)
                                          (&&)
                                          (&&)

-- 1.B.III

simplificarExpA' :: ExpA -> ExpA
simplificarExpA' = foldExpA (\n -> Cte n)
                            simplificarSuma
                            simplificarProd

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
showExpA = foldExpA (\n -> "(Cte " ++ (show n) ++ ")")
                    (\s1 s2 -> "(Suma " ++ s1 ++ s2 ++ ")")
                    (\s1 s2 -> "(Prod " ++ s1 ++ s2 ++ ")")

-- 1.C

¿evalExpA' = evalExpA?

-- 1.D

recExpA :: (Int -> b) -> (ExpA -> b -> ExpA -> b -> b) -> (ExpA -> b -> ExpA -> b -> b) -> ExpA -> b
recExpA f g h (Cte n)      = f n
recExpA f g h (Suma e1 e2) = g e1 (recExpA f g h e1) e2 (recExpA f g h e2)
recExpA f g h (Prod e1 e2) = h e1 (recExpA f g h e1) e2 (recExpA f g h e2)

-- 1.E.I

cantDeSumaCeros :: ExpA -> Int
cantDeSumaCeros = fst . foldExpA fCte fSuma fProd
    where fCte  n                 = (0, n == 0)
          fSuma (n1, b1) (n2, b2) = let cantCero = if b1 || b2 then 1 else 0
                                    in (cantCero + n1 + n2, False)
          fProd (n1, _ ) (n2, _ ) = (n1 + n2, False)

-- 1.E.II

cantDeProdUnos :: ExpA -> Int
cantDeProdUnos = fst . foldExpA fCte fSuma fProd
    where fCte  n                 = (0, n == 1)
          fSuma (n1, _ ) (n2, _ ) = (n1 + n2, False)
          fProd (n1, b1) (n2, b2) = let cantUnos = if b1 || b2 then 1 else 0
                                    in (cantUnos + n1 + n2, False)