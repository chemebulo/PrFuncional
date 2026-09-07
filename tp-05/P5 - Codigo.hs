--------------------------------------------------------------------------------------------------------

--                               TRABAJO PRÁCTICO N°5: Tipos Algebraicos                              --

--------------------------------------------------------------------------------------------------------

> Ejercicio 1:

data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon

data Helado = Vasito Gusto
            | Cucurucho Gusto Gusto
            | Pote Gusto Gusto Gusto

chocoHelate consH = consH Chocolate

-- 1.A

Vasito :: ...


-- 1.B

Chocolate :: ...


-- 1.C

Cucurucho :: ...


-- 1.D

Sambayon :: ...


-- 1.E

Pote :: ...


-- 1.F

chocoHelate :: ...


-- 1.G

chocoHelate Vasito :: ...


-- 1.H

chocoHelate Cucurucho :: ...


-- 1.I

chocoHelate (Cucurucho Sambayon) :: ...


-- 1.J

chocoHelate (chocoHelate Cucurucho) :: ...


-- 1.K

chocoHelate (Vasito DulceDeLeche) :: ...


-- 1.L

chocoHelate Pote :: ...


-- 1.M

chocoHelate (chocoHelate (Pote Frutilla))


> Ejercicio 2:

data DigBin = O | I

-- 2.A

dbAsInt :: DigBin -> Int


-- 2.B

dbAsBool :: DigBin -> Bool


-- 2.C

dbOfBool :: Bool -> DigBin


-- 2.D

negDB :: DigBin -> DigBin


> Ejercicio 3:

data DigDec = D0 | D1 | D2 | D3 | D4
            | D5 | D6 | D7 | D8 | D9

-- 3.A

ddAsInt :: DigDec -> Int


-- 3.B

ddOfInt :: Int -> DigDec


-- 3.C

nextDD :: DigDec -> DigDec


-- 3.D

prevDD :: DigDec -> DigDec


> Ejercicio 4:

data Medida = Mm   Float | Cm   Float
            | Inch Float | Foot Float


-- 4.A

asMm :: Medida -> Medida


-- 4.B

asCm :: Medida -> Medida


-- 4.C

asInch :: Medida -> Medida


-- 4.D

asFoot :: Medida -> Medida


> Ejercicio 5:

data Shape = Circle Float | Rect Float Float

construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0


-- 5.A

uncurry Rect :: ...


-- 5.B

construyeShNormal (flip Rect 5.0) :: ...


-- 5.C

compose (uncurry Rect) swap :: ...


-- 5.D

uncurry Cucurucho :: ...


-- 5.E

uncurry Rect swap :: ...


-- 5.F

compose uncurry Pote :: ...


-- 5.G

compose Just :: ...


-- 5.H

compose uncurry (Pote Chocolate) :: ...


> Ejercicio 6:

-- 6.A




-- 6.B




-- 6.C




-- 6.D




-- 6.E




-- 6.F




-- 6.G




-- 6.H


> Ejercicio 7:

data Set a = S (a -> Bool)

-- 7.A

belongs :: Set a -> a -> Bool


-- 7.B

empty :: Set a


-- 7.C

singleton :: a -> Set a


-- 7.D

union :: Set a -> Set a -> Set a


-- 7.E

intersection :: Set a -> Set a -> Set a


> Ejercicio 8:

data MayFail a = Raise Exception | Ok a

data Exception = DivByZero | NotFound | NullPointer
               | Other String

type ExHandler a = Exception -> a

mostrarInt :: Int -> GUI Int

ventanaError :: String -> GUI a

lookupE :: Nombre -> [Empleado] -> MayFail Int

-- 8.A

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b