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
evalEA (Const n)        =
evalEA (BOp bo ea1 ea2) =

-- 1.A.II

ea2ExpA :: EA -> ExpA
ea2ExpA (Const n)        =
ea2ExpA (BOp bo ea1 ea2) =

-- 1.A.III

expA2ea :: ExpA -> EA
expA2ea (Cte n)      =
expA2ea (Suma e1 e2) =
expA2ea (Prod e1 e2) =


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

-- 2.A.II

cantidadDeNodos :: Arbol a b -> Int


-- 2.A.III

cantidadDeConstructores :: Arbol a b -> Int


-- 2.A.IV

ea2Arbol :: EA -> Arbol BinOp Int


-- 2.B

¿para todo t :: Arbol a b. cantidadDeHojas t + cantidadDeNodos t = cantidadDeConstructores t?





> Ejercicio 3:

data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a)
    deriving Show


-- 3.A


-- 3.A




-- 3.A





-- 3.A




-- 3.A





-- 3.A





-- 3.A




-- 3.A




-- 3.A




-- 3.A




-- 3.A




-- 3.A



> Ejercicio 4:






> Ejercicio 5:

