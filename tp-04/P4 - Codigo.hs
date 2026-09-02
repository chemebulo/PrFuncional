--------------------------------------------------------------------------------------------------------

--                                   TRABAJO PRÁCTICO N°4: Reducción                                  --

--------------------------------------------------------------------------------------------------------

> Ejercicio 1:

-- 1.A

udiv (x,y) = div x y

-- 1.B

udivE (x,0) = error "No puedo dividir por 0" 
udivE (x,y) = div x y

-- 1.C

udivH = uncurry div

-- 1.D

succ x = x + 1

-- 1.E

succH = suma 1 

-- 1.F

porLaMitad = flip div 2

-- 1.G

conDieresis 'u' = 'ü'

-- 1.H

conDieresisB 'u' = 'ü' 
conDieresisB c   = conDieresisB c 

-- 1.I

conTildePM 'a' = 'á' 
conTildePM 'e' = 'é' 
conTildePM 'i' = 'í' 
conTildePM 'o' = 'ó' 
conTildePM 'u' = 'ú'

-- 1.J

conTildeE c = if esVocal c 
                 then conTildePM c 
                 else error "El valor recibido no es vocal"

-- 1.K

conTilde c = if esVocal c && esMinuscula c 
                then conTildePM c 
                else c

> Ejercicio 2:



> Ejercicio 3:

-- 3.A

twice doble

-- 3.B

twice doble 2

-- 3.C

twice

> Ejercicio 4:

-- 4.A



-- 4.B



-- 4.C



> Ejercicio 5:

-- 5.A



-- 5.B



-- 5.C



> Ejercicio 6:

-- 6.A



-- 6.B



-- 6.C

