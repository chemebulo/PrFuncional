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

data AppList a = Single a
               | Append (AppList a) (AppList a)
    deriving Show

foldAL :: (a -> b) -> (b -> b -> b) -> AppList a -> b
foldAL fs fa (Single x)       = fs x
foldAL fs fa (Append ali ald) = fa (foldAL fs fa ali) (foldAL fs fa ald)

recAL :: (a -> b) -> (AppList a -> b -> AppList a -> b -> b) -> AppList a -> b
recAL fs fa (Single x)       = fs x
recAL fs fa (Append ali ald) = fa ali (recAL fs fa ali) ald (recAL fs fa ald)

-- 4.A.I

lenAL :: AppList a -> Int
lenAL = foldAL (const 1) (+)

-- 4.A.II

consAL :: a -> AppList a -> AppList a
consAL x =  recAL Single fAppend
    where fAppend ali _ ald _ = Append (Single x) (Append ali ald)

consALV2 :: a -> AppList a -> AppList a
consALV2 x al = Append (Single x) al 

-- 4.A.III

headAL :: AppList a -> a
headAL = foldAL id fAppend
    where fAppend x _ = x

-- 4.A.IV

tailAL :: AppList a -> AppList a
tailAL = recAL (const (error "No se puede quitar el elemento, ya que no se puede representar una lista vacía")) fAppend
    where fAppend (Single x) _    ald _ = ald;
          fAppend _          ali' ald _ = Append ali' ald 

tailALV2 :: AppList a -> AppList a
tailALV2 = recAL (const (error "No se puede quitar el elemento, ya que no se puede representar una lista vacía")) fAppend
    where fAppend ali ali' ald _ = case ali of
                                     (Single x) -> ald
                                     _          -> Append ali' ald

-- 4.A.V

snocAL :: AppList a -> a -> AppList a
snocAL = recAL fSingle fAppend
    where fSingle x            y = Append (Single x) (Single y)
          fAppend ali _ _ ald' y = Append ali (ald' y)

-- 4.A.VI

lastAL :: AppList a -> a
lastAL = foldAL id fAppend
    where fAppend _ x = x

-- 4.A.VII

initAL :: AppList a -> AppList a
initAL = recAL (const (error "No se puede quitar el elemento, ya que no se puede representar una lista vacía")) fAppend
    where fAppend ali _ ald ald' = case ald of
                                     (Single x) -> ali
                                     _          -> Append ali ald'

-- 4.A.VIII

reverseAL :: AppList a -> AppList a
reverseAL = foldAL Single fAppend 
    where fAppend ali' ald' = Append ald' ali'

-- 4.A.IX

elemAL :: Eq a => a -> AppList a -> Bool
elemAL x = foldAL (x==) (||)

-- 4.A.X

appendAL :: AppList a -> AppList a -> AppList a
appendAL al1 al2 = Append al1 al2

appendALV2 :: AppList a -> AppList a -> AppList a
appendALV2 = recAL fSingle fAppend
    where fSingle x            al2 = Append (Single x) al2 
          fAppend ali _ _ ald' al2 = Append ali (ald' al2)

-- 4.A.XI

appListToList :: AppList a -> [a]
appListToList = foldAL (:[]) (++)


> Ejercicio 5:

data QuadTree a = LeafQ a
                | NodeQ (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
    deriving Show

data Color = RGB Int Int Int
    deriving Show

type Image = QuadTree Color

foldQT :: (a -> b) -> (b -> b -> b -> b -> b) -> QuadTree a -> b
foldQT fl fn (LeafQ x)           = fl x
foldQT fl fn (NodeQ q1 q2 q3 q4) = fn (foldQT fl fn q1) (foldQT fl fn q2) (foldQT fl fn q3) (foldQT fl fn q4)

recQT :: (a -> b) -> (QuadTree a -> b -> QuadTree a -> b -> QuadTree a -> b -> QuadTree a -> b -> b) -> QuadTree a -> b
recQT fl fn (LeafQ x)           = fl x
recQT fl fn (NodeQ q1 q2 q3 q4) = fn q1 (recQT fl fn q1)
                                     q2 (recQT fl fn q2)
                                     q3 (recQT fl fn q3)
                                     q4 (recQT fl fn q4)

-- 5.A.I

heightQT :: QuadTree a -> Int
heightQT = foldQT (const 0) fNodeQ
    where fNodeQ h1 h2 h3 h4 = 1 + (max h1 $ max h2 $ max h3 h4) 

-- 5.A.II

countLeavesQT :: QuadTree a -> Int
countLeavesQT = foldQT (const 1) fNodeQ
    where fNodeQ n1 n2 n3 n4 = n1 + n2 + n3 + n4

-- 5.A.III

sizeQT :: QuadTree a -> Int
sizeQT = foldQT (const 1) fNodeQ
    where fNodeQ n1 n2 n3 n4 = 1 + n1 + n2 + n3 + n4

-- 5.A.IV

compress :: Eq a => QuadTree a -> QuadTree a
compress = recQT LeafQ fNodeQ
    where fNodeQ (LeafQ x1) q1' (LeafQ x2) q2' (LeafQ x3) q3' (LeafQ x4) q4' =
            if x1 == x2 && x2 == x3 && x3 == x4
			   then LeafQ x1
			   else NodeQ (LeafQ x1) (LeafQ x2) (LeafQ x3) (LeafQ x4);
		  fNodeQ _ q1' _ q2' _ q3' _ q4' = NodeQ q1' q2' q3' q4'

-- 5.A.V

uncompress :: QuadTree a -> QuadTree a
uncompress qt = uncompress' qt (0, heightQT qt)

uncompress' :: QuadTree a -> (Int, Int) -> QuadTree a
uncompress' = foldQT fLeafQ fNodeQ
    where fLeafQ x (ha, hqt) = if ha == hqt
                                  then LeafQ x
                                  else NodeQ (LeafQ x) (LeafQ x) (LeafQ x) (LeafQ x);
          fNodeQ q1' q2' q3' q4' (ha, hqt) = NodeQ (q1' (ha+1, hqt))
                                                   (q2' (ha+1, hqt))
                                                   (q3' (ha+1, hqt))
                                                   (q4' (ha+1, hqt))  

-- 5.A.VI

render :: Image -> Int -> Image
render img t = uncompress' img (0, log4 t)

log4 :: Int -> Int
log4 1 = 0
log4 n = 1 + (log4 $ div n 4)


> Funciones Alternativas a las Propuestas:

> CountLeaves:

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

> LevelN:

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