--------------------------------------------------------------------------------------------------------

--                      TRABAJO PRÁCTICO N°9: Inducción y Recursión Estructural III                   --

--------------------------------------------------------------------------------------------------------

> Ejercicio 1:

data ExpA = Cte Int
          | Suma ExpA ExpA 
          | Prod ExpA ExpA
    deriving Show

data EA = Const Int | BOp BinOp EA EA
    deriving Show

data BinOp = Sum | Mul
    deriving Show

-- 1.A.I

evalEA :: EA -> Int
evalEA (Const n)        = n
evalEA (BOp bo ea1 ea2) = evalBO bo (evalEA ea1) (evalEA ea2)

evalBO :: BinOp -> Int -> Int -> Int
evalBO Sum n m = n + m  
evalBO Mul n m = n * m 

-- 1.A.II

ea2ExpA :: EA -> ExpA
ea2ExpA (Const n)        = Cte n
ea2ExpA (BOp bo ea1 ea2) = bo2ExpA bo (ea2ExpA ea1) (ea2ExpA ea2)

bo2ExpA :: BinOp -> ExpA -> ExpA -> ExpA
bo2ExpA Sum e1 e2 = Suma e1 e2
bo2ExpA Mul e1 e2 = Prod e1 e2

-- 1.A.III

expA2ea :: ExpA -> EA
expA2ea (Cte n)      = Const n
expA2ea (Suma e1 e2) = BOp Sum (expA2ea e1) (expA2ea e2)
expA2ea (Prod e1 e2) = BOp Mul (expA2ea e1) (expA2ea e2)


-- 1.B.I

ea2ExpA . expA2ea = id

-- 1.B.II

expA2ea . ea2ExpA = id


-- 1.B.III

evalExpA . ea2ExpA = evalEA



-- 1.B.IV

evalEA . expA2ea = evalExpA



> Ejercicio 2:

data Arbol a b = Hoja b
               | Nodo a (Arbol a b) (Arbol a b)
    deriving Show

-- 2.A.I

cantidadDeHojas :: Arbol a b -> Int
cantidadDeHojas (Hoja _)         = 1
cantidadDeHojas (Nodo _ ab1 ab2) = cantidadDeHojas ab1 + cantidadDeHojas ab2

-- 2.A.II

cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja _)         = 0
cantidadDeNodos (Nodo _ ab1 ab2) = 1 + cantidadDeNodos ab1 + cantidadDeNodos ab2

-- 2.A.III

cantidadDeConstructores :: Arbol a b -> Int
cantidadDeConstructores (Hoja _)         = 1
cantidadDeConstructores (Nodo _ ab1 ab2) = 1 + cantidadDeConstructores ab1 + cantidadDeConstructores ab2

-- 2.A.IV

ea2Arbol :: EA -> Arbol BinOp Int
ea2Arbol (Const n)        = Hoja n
ea2Arbol (BOp bo ea1 ea2) = Nodo bo (ea2Arbol ea1) (ea2Arbol ea2)


-- 2.B

¿para todo t :: Arbol a b. cantidadDeHojas t + cantidadDeNodos t = cantidadDeConstructores t?



> Ejercicio 3:

data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a)
    deriving Show

-- 3.A.I

sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT n ti td) = n + sumarT ti + sumarT td

-- 3.A.II

sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT _ ti td) = 1 + sizeT ti + sizeT td  

-- 3.A.III

anyT :: (a -> Bool) -> Tree a -> Bool
anyT f EmptyT          = False
anyT f (NodeT x ti td) = f x || anyT f ti || anyT f td

-- 3.A.IV

countT :: (a -> Bool) -> Tree a -> Int
countT f EmptyT          = 0
countT f (NodeT x ti td) = unoSi (f x) + countT f ti + countT f td 

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

-- 3.A.V

countLeaves :: Tree a -> Int
countLeaves EmptyT                  = 0
countLeaves (NodeT _ EmptyT EmptyT) = 1 
countLeaves (NodeT _ ti     td)     = countLeaves ti + countLeaves td

-- 3.A.VI

heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT _ ti td) = 1 + max (heightT ti) (heightT td)

-- 3.A.VII

inOrder :: Tree a -> [a]
inOrder EmptyT          = []
inOrder (NodeT x ti td) = inOrder ti ++ [x] ++ inOrder td

-- 3.A.VIII

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x ti td) = [x] : mergeLevel (listPerLevel ti) (listPerLevel td)

mergeLevel :: [[a]] -> [[a]] -> [[a]]
mergeLevel []       yss      = yss
mergeLevel xss      []       = xss
mergeLevel (xs:xss) (ys:yss) = (xs ++ ys) : mergeLevel xss yss  

-- 3.A.IX

mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x ti td) = NodeT (mirrorT td) (mirrorT ti)

-- 3.A.X

levelN :: Int -> Tree a -> [a]
levelN n EmptyT          = []
levelN 0 (NodeT x _   _) = [x]
levelN n (NodeT _ ti td) = levelN (n-1) ti ++ levelN (n-1) td

-- 3.A.XI

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x ti td) = x : if length (ramaMasLarga ti) >= length (ramaMasLarga td)
                                      then ramaMasLarga ti
                                      else ramaMasLarga td

-- 3.A.XII

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x ti td) = [x] : agregarACaminos x (todosLosCaminos ti) ++ agregarACaminos x (todosLosCaminos td)

agregarACaminos :: a -> [[a]] -> [[a]] -> [[a]]
agregarACaminos x []       = []
agregarACaminos x (xs:xss) = (x:xs) : agregarACaminos x xss 


-- 3.B.I

¿heightT = length . ramaMasLarga?


-- 3.B.I

¿reverse . inOrder = inOrder . mirrorT?



> Ejercicio 4:






> Ejercicio 5:




> FUNCIONES ALTERNATIVAS A LAS PROPUESTAS:

-- CountLeaves:

countLeavesV2 :: Tree a -> Int
countLeavesV2 EmptyT          = 0
countLeavesV2 (NodeT _ ti td) = unoSi (areLeaves ti td) + countLeavesV2 ti + countLeavesV2 td

areLeaves :: Tree a -> Tree a -> Bool
areLeaves EmptyT EmptyT = True
areLeaves _      _      = False

countLeavesV3 :: Tree a -> Int
countLeavesV3 EmptyT          = 0
countLeavesV3 (NodeT _ ti td) = case (ti, td) of
                                    (EmptyT, EmptyT) -> 1
                                    _                -> countLeavesV3 ti + countLeavesV3 td

-- LevelN:

levelNV2 :: Int -> Tree a -> [a]
levelNV2 n EmptyT          = []
levelNV2 n (NodeT x ti td) = if n > 0
                                then levelNV2 (n-1) ti ++ levelNV2 (n-1) td
                                else [x]

levelNV3 :: Int -> Tree a -> [a]
levelNV3 0 t = case t of 
                EmptyT        -> []
                (NodeT x _ _) -> [x]
levelNV3 n t = case t of
                EmptyT          -> []
                (NodeT _ ti td) -> levelNV3 (n-1) ti ++ levelNV3 (n-1) td