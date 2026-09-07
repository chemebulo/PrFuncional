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

Vasito :: Gusto -> Helado


-- 1.B

Chocolate :: Gusto


-- 1.C

Cucurucho :: Gusto -> Gusto -> Helado


-- 1.D

Sambayon :: Gusto


-- 1.E

Pote :: Gusto -> Gusto -> Gusto -> Helado


-- 1.F

chocoHelate :: (Gusto -> a) -> a


-- 1.G

chocoHelate Vasito :: Helado


-- 1.H

chocoHelate Cucurucho :: Gusto -> Helado


-- 1.I

chocoHelate (Cucurucho Sambayon) :: Helado


-- 1.J

chocoHelate (chocoHelate Cucurucho) :: Helado


-- 1.K

chocoHelate (Vasito DulceDeLeche) :: No tiene tipo.


-- 1.L

chocoHelate Pote :: Gusto -> Gusto -> Helado


-- 1.M

chocoHelate (chocoHelate (Pote Frutilla)) :: Helado


> Ejercicio 2:

data DigBin = O | I

-- 2.A

dbAsInt :: DigBin -> Int
dbAsInt I = 1
dbAsInt O = 0


-- 2.B

dbAsBool :: DigBin -> Bool
dbAsBool I = True
dbAsBool O = False


-- 2.C

dbOfBool :: Bool -> DigBin
dbOfBool True  = I
dbOfBool False = O


-- 2.D

negDB :: DigBin -> DigBin
negDB I = O
negDB O = I


> Ejercicio 3:

data DigDec = D0 | D1 | D2 | D3 | D4
            | D5 | D6 | D7 | D8 | D9

-- 3.A

ddAsInt :: DigDec -> Int
ddAsInt D0 = 0
ddAsInt D1 = 1
ddAsInt D2 = 2
ddAsInt D3 = 3
ddAsInt D4 = 4
ddAsInt D5 = 5
ddAsInt D6 = 6
ddAsInt D7 = 7 
ddAsInt D8 = 8
ddAsInt D9 = 9


-- 3.B

ddOfInt :: Int -> DigDec
ddOfInt 0 = D0
ddOfInt 1 = D1
ddOfInt 2 = D2
ddOfInt 3 = D3
ddOfInt 4 = D4
ddOfInt 5 = D5
ddOfInt 6 = D6
ddOfInt 7 = D7
ddOfInt 8 = D8
ddOfInt 9 = D9


-- 3.C

nextDD :: DigDec -> DigDec
nextDD D0 = D1
nextDD D1 = D2
nextDD D2 = D3 
nextDD D3 = D4
nextDD D4 = D5
nextDD D5 = D6
nextDD D6 = D7
nextDD D7 = D8
nextDD D8 = D9
nextDD D9 = D0


-- 3.D

prevDD :: DigDec -> DigDec
prevDD D0 = D9
prevDD D1 = D0
prevDD D2 = D1
prevDD D3 = D2
prevDD D4 = D3
prevDD D5 = D4
prevDD D6 = D5
prevDD D7 = D6
prevDD D8 = D7
prevDD D9 = D8


> Ejercicio 4:

data Medida = Mm   Float | Cm   Float
            | Inch Float | Foot Float


-- 4.A

asMm :: Medida -> Medida
asMm (Mm n)   = 
asMm (Cm n)   =
asMm (Inch n) =
asMm (Foot n) =


-- 4.B

asCm :: Medida -> Medida
asCm (Mm n)   = 
asCm (Cm n)   =
asCm (Inch n) =
asCm (Foot n) =


-- 4.C

asInch :: Medida -> Medida
asInch (Mm n)   = 
asInch (Cm n)   =
asInch (Inch n) =
asInch (Foot n) =


-- 4.D

asFoot :: Medida -> Medida
asFoot (Mm n)   = 
asFoot (Cm n)   =
asFoot (Inch n) =
asFoot (Foot n) =


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