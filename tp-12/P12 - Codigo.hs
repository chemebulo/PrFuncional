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


> Ejercicio 2:

data EA = Const Int | BOp BinOp EA EA
    deriving Show

data BinOp = Sum | Mul
    deriving Show

-- 2.A

foldEA :: (Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b
foldEA f g (Const n)        = f n
foldEA f g (BOp bo ea1 ea2) = g bo (foldEA f g ea1) (foldEA f g ea1)

-- 2.B.I

noTieneNegativosExplicitosEA :: EA -> Bool
noTieneNegativosExplicitosEA = foldEA (>= 0) fBOp
    where fBOp bo ea1' ea2' = ea1' && ea2'

-- 2.B.II

simplificarEA' :: EA -> EA
simplificarEA' = foldEA (\n -> Const n) fBOp
    where fBOp Sum ea1' ea2' = simplificarSum ea1' ea2'
          fBOp Mul ea1' ea2' = simplificarMul ea1' ea2'

simplificarSum :: EA -> EA -> EA
simplificarSum (Const 0) ea2       = ea2
simplificarSum ea1       (Const 0) = ea1
simplificarSum ea1       ea2       = BOp Sum ea1 ea2


simplificarMul :: EA -> EA -> EA
simplificarMul (Const 0) ea2       = ea2
simplificarMul (Const 1) ea2       = ea2
simplificarMul ea1       (Const 0) = ea1
simplificarMul ea1       (Const 1) = ea1
simplificarMul ea1       ea2       = BOp Mul ea1 ea2

-- 2.B.III

evalEA' :: EA -> Int
evalEA' = foldEA id fBOp
    where fBOp Sum ea1' ea2' = ea1' + ea2'
          fBOp Mul ea1' ea2' = ea1' * ea2'


-- 2.B.IV

showEA :: EA -> String


-- 2.B.V

ea2ExpA' :: EA -> ExpA
ea2ExpA' = foldEA (\n -> Cte n) fBOp
    where fBOp Sum e1' e2' = Suma e1' e2'
          fBOp Mul e1' e2' = Prod e1' e2'

-- 2.B.VI

ea2Arbol' :: EA -> ABTree BinOp Int


-- 2.C

¿evalEA' = evalEA?


> Ejercicio 3:

data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a)
    deriving Show

-- 3.A

foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
foldT z f EmptyT          = b
foldT z f (NodeT x ti td) = f x (foldT z f ti) (foldT z f td)

-- 3.B.I

mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT EmptyT fNodeT
    where fNodeT x ti td = NodeT (f x) ti td

-- 3.B.II

sumT :: Tree Int -> Int
sumT = foldT 0 fNodeT
    where fNodeT n ni nd = n + ni + nd

-- 3.B.III

sizeT :: Tree a -> Int
sizeT = foldT 0 fNodeT
    where fNodeT _ n1 n2 = 1 + n1 + n2

-- 3.B.IV

heightT :: Tree a -> Int
heightT = foldT 0 fNodeT
    where fNodeT _ n1 n2 = 1 + max n1 n2 

-- 3.B.V

preOrder :: Tree a -> [a]
preOrder = foldT [] fNodeT
    where fNodeT x xsi xsd = x : xsi ++ xsd

-- 3.B.VI

inOrder :: Tree a -> [a]
inOrder = foldT [] fNodeT
    where fNodeT x xsi xsd = xsi ++ [x] ++ xsd

-- 3.B.VII

postOrder :: Tree a -> [a]
inOrder = foldT [] fNodeT
    where fNodeT x xsi xsd = xsi ++ xsd ++ [x]

-- 3.B.XIII

mirrorT :: Tree a -> Tree a
mirrorT = foldT EmptyT fNodeT
    where fNodeT x ti td = NodeT x td ti

-- 3.B.IX

countByT :: (a -> Bool) -> Tree a -> Int
countByT f = foldT 0 fNodeT
    where fNodeT x n1 n2 = unoSi (f x) + n1 + n2

-- 3.B.X

partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
partitionT f = foldT ([], []) fNodeT
    where fNodeT x pi pd = let (xsi1, xsd1) = pi;
                               (xsi2, xsd2) = pd
                           in if f x then (x : xsi1 ++ xsi2, xsd1 ++ xsd2)
                                     else (xsi1 ++ xsi2, x : xsd1 ++ xsd2)

-- 3.B.XI

zipWithT :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipWithT f = foldT (const EmptyT) fNodeT
    where fNodeT x fti ftd (NodeT y t2i t2d) = NodeT (f x y) (fti t2i) (ftd t2d)
          fNodeT _ _   _   _                 = EmptyT


-- 3.B.XII

caminoMasLargo :: Tree a -> [a]
caminoMasLargo = foldT [] fNodeT
    where fNodeT x xsi xsd = x : if length xsi >= length xsd
                                    then xsi
                                    else xsd

-- 3.B.XIII

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos = foldT [] fNodeT
    where fNodeT x xssi xssd = [x] : (map (x:) (xssi ++ xssd))

-- 3.B.XIV

todosLosNiveles :: Tree a -> [[a]]
todosLosNiveles = foldT [] fNodeT
    where fNodeT x xssi xssd = [x] : mergeLevel xssi xssd

mergeLevel :: [[a]] -> [[a]] -> [[a]]
mergeLevel []       yss      = yss
mergeLevel xss      []       = xss
mergeLevel (xs:xss) (ys:yss) = (xs ++ ys) : mergeLevel xss yss  

-- 3.B.XV

nivelN :: Tree a -> Int -> [a]
nivelN = foldT (\n -> []) fNodeT
    where fNodeT x _    _    0 = [x]
          fNodeT _ fxsi fxsd n = fxsi (n-1) ++ fxsd (n-1)

-- 3.C

recT :: b -> (a -> Tree a -> b -> Tree a -> b -> b) -> Tree a -> b
recT z f EmptyT          = z
recT z f (NodeT x ti td) = f x ti (recT z f ti) td (recT z f td)

-- 3.D.I

insertT :: a -> Tree a -> Tree a
insertT x = recT (NodeT x EmptyT EmptyT) fNodeT
    where fNodeT y ti ti' td td' = case (compare x y) of
                                     EQ -> NodeT y ti td
                                     LT -> NodeT y ti' td
                                     GT -> NodeT y ti td'

-- 3.D.II

caminoHasta :: Ord a => a -> Tree a -> [a]
caminoHasta x = recT [] fNodeT
    where fNodeT y ti xsi td xsd = case (compare x y) of
		 							 EQ -> [y]
									 LT -> y : xsi
									 GT -> y : xsd