-- CONSIGNA:
   -- La Terminal debilita el ataque restando su potencia.
   -- El Servidor aplifica el ataque sumando su potencia.
   -- El Firewall determina mediante su predicado si un ataque es suficiente para vulnerar el sistema.
   -- El Switch mediante su predicado determina a cuál de los dos nodos se accede primero, en caso de ambigüedad accede primero a la izquierda.

Type Potencia = Int
Type Ataque = Int

Data Dir = Izq | Der | Forward

Type Ruta = [Dir]

Data Red = Terminal Potencia
         | Servidor Potencia Red
         | Firewall (Ataque -> Bool) Red
         | Switch (Ataque -> Dir) Red Red


-- EJERCICIO 1: Definir las siguientes funciones con recursión explícita.

combinarFirewalls :: Red -> Red

intensidadRestante :: Ataque -> Red -> Maybe Ataque

pasosHastaBloqueo :: Ataque -> Red -> Maybe Int

rutasAServidores :: Ataque -> Red -> [Ruta]


-- EJERCICIO 2: Demostrar la siguiente propiedad.

Para todo i, intensidadRestante i . combinarFirewalls = intensidadRestante i

Ayuda 1. (No me acuerdo que decía aca)

Ayuda 2. Considerar la siguiente propiedad ya demostrada:
Para todo b1, b2, e1, e2. if b1 then (if b2 then e1 else e2) else e2 = if b1 && b2 then e1 else e2


-- EJERCICIO 3: Dar tipo y definir foldRed y recRed, una versión de fold y recursión primitiva, respectivamente, para la estructura Red.

foldRed :: (Potencia -> b) -> (Potencia -> b -> b) -> ((Ataque -> Bool) -> b -> b) -> ((Ataque -> Dir) -> b -> b -> b) -> Red -> b

recRed :: (Potencia -> b) -> (Potencia -> Red -> b -> b) -> ((Ataque -> Bool) -> Red -> b -> b) -> ((Ataque -> Dir) -> Red -> b -> Red -> b -> b) -> Red -> b


-- EJERCICIO 4: Definir las funciones del primer punto usando foldRed o recRed, según corresponda.

combinarFirewalls :: Red -> Red

intensidadRestante :: Ataque -> Red -> Maybe Ataque

pasosHastaBloqueo :: Ataque -> Red -> Maybe Int

rutasAServidores :: Ataque -> Red -> [Ruta]