> Funciones Útiles para el Parcial:

      compare :: Ord a => a -> a -> Ordering 
      compare x y = if x == y
                       then EQ
                       else if x >= y
                               then GT
                               else LT

            caminoHasta :: Ord a => a -> Tree a -> [a]
            caminoHasta x = recT [] fNodeT
                  where fNodeT y ti xsi td xsd = case (compare x y) of
                                                   EQ -> [y]
                                                   LT -> y : xsi
                                                   GT -> y : xsd

      maybe :: a -> (b -> a) -> Maybe b -> a
      maybe _ f (Just x) = f x
      maybe z _ Nothing  = z

            findAtAndTransform :: a -> (b -> a) -> b -> Tree b -> a 
            findAtAndTransform z f x t = maybe z f (findAt x t)

            findAt :: Eq a => a -> Tree a -> Maybe a

      transformMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
      transformMaybe f (Just x) my = Just (maybe x (f x) my)
      transformMaybe _ Nothing  my = my

            cantidadAntecesores :: Eq a => a -> Tree a -> Maybe Int 
            cantidadAntecesores z EmptyT          = Nothing
            cantidadAntecesores z (NodeT x ti td) = if x == z
                                                       then Just 1
                                                       else transformMaybe (+) (cantidadAntecesores z ti)
                                                                               (cantidadAntecesores z td) 

      elegirEntre :: Maybe a -> Maybe a -> Maybe a
      elegirEntre (Just x) m = Just x
      elegirEntre Nothing  m = m

            findAt :: Eq a => a -> Tree a -> Maybe a
            findAt z EmptyT          = Nothing
            findAt z (NodeT x ti td) = if x == z
                                          then Just x
                                          else elegirEntre (findAt z ti) (findAt z td)

            elegirEntre :: Maybe a -> Maybe a -> Maybe a
            elegirEntre (Just x) m = Just x
            elegirEntre Nothing  m = m

      agregarA :: a -> Maybe [a] -> Maybe [a]
      agregarA y Nothing   = Nothing
      agregarA y (Just xs) = Just (y:xs)

            filterBy :: (a -> Bool) -> [a] -> Maybe [a]
            filterBy f []     = Nothing
            filterBy f (x:xs) = if f x
                                   then agregarA x (filterBy f xs)
                                   else filterBy f xs


> Funciones Fundamentales:

      many :: Int -> (a -> a) -> a -> a 
      many 0 f = id
      many n f = f . (many (n-1) f)

      twice :: (a -> a) -> a -> a
      twice f x = f (f x)

      flip :: (a -> b -> c) -> b -> a -> c
      flip f x y = f y x

      uflip :: ((a, b) -> c) -> (b, a) -> c
      uflip f (x, y) = f (y, x)

      subst :: (a -> b -> c) -> (a -> b) -> a -> c
      subst f g x = (f x) (g x)

      compose :: (a -> b) -> (c -> a) -> c -> b
      compose f g x = f (g x)

      compose21 :: (a -> b -> c) -> a -> (d -> b) -> d -> c 
      compose21 f x g y = f x (g y)

      compose12 :: (d -> c) -> (a -> b -> d) -> a -> b -> c
      compose12 f g x y = f (g x y)

      curry :: ((a, b) -> c) -> a -> b -> c
      curry f x y = f (x, y)

      uncurry :: (a -> b -> c) -> (a, b) -> c
      uncurry f (x, y) = f x y

      dup :: a -> (a, a)
      dup x = (x, x)

      apply :: (a -> b) -> a -> b
      apply f x = f x

      appDup :: ((a, a) -> b) -> a -> b
      appDup f x = f (x , x)

      appFork :: (a -> b , a -> c) -> a -> (b , c)
      appFork (f, g) x = (f x, g x)

      appPar :: (a -> b , c -> d) -> (a , c) -> (b , d)
      appPar (f, g) (x, y) = (f x, g y)

      appDist :: (a -> b) -> (a , a) -> (b , b)
      appDist f (x, y) = (f x, f y)

      assoc :: (a, (b, c)) -> ((a, b), c)
      assoc (x, (y, z)) = ((x, y), z)

      appAssoc :: (((a, b), c) -> d) -> (a, (b, c)) -> d
      appAssoc f p = f (assoc p)

      scanr :: (a -> b -> b) -> b -> [a] -> [b]
      scanr _ z []     = [z]
      scanr f z (x:xs) = let (y:ys) = scanr f z xs in f x y : (y:ys)


> Funciones y Operadores Interesantes:

      > Funciones:

            zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
            zipWith f = foldr ctk (\_ -> [])
                  where ctk x xs' (y:ys) = f x y : xs' ys';
                        ctk _ _   []     = []

            nivelN :: Tree a -> Int -> [a]
            nivelN = foldT (const []) fNodeT
                  where fNodeT x _    _    0 = [x]
                        fNodeT _ fxsi fxsd n = fxsi (n-1) ++ fxsd (n-1)

            appendAL :: AppList a -> AppList a -> AppList a
            appendAL = recAL fSingle fAppend
                  where fSingle x            al2 = Append (Single x) al2 
                        fAppend ali _ _ ald' al2 = Append ali (ald' al2)

            partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
            partitionT f = foldT ([], []) fNodeT
                  where fNodeT x pi pd = let (xsi1, xsd1) = pi;
                                             (xsi2, xsd2) = pd
                                         in if f x
                                               then (x : xsi1 ++ xsi2, xsd1 ++ xsd2)
                                               else (xsi1 ++ xsi2, x : xsd1 ++ xsd2)

            caminoMasLargo :: Tree a -> [a]
            caminoMasLargo = foldT [] fNodeT
                  where fNodeT x xsi xsd = x : if length xsi >= length xsd
                                                  then xsi
                                                  else xsd

            todosLosCaminos :: Tree a -> [[a]]
            todosLosCaminos = foldT [] fNodeT
                  where fNodeT x xssi xssd = [x] : map (x:) (xssi ++ xssd)

            todosLosNiveles :: Tree a -> [[a]]
            todosLosNiveles = foldT [] fNodeT
                  where fNodeT x xssi xssd = [x] : mergeLevel xssi xssd

            mergeLevel :: [[a]] -> [[a]] -> [[a]]
            mergeLevel []       yss      = yss
            mergeLevel xss      []       = xss
            mergeLevel (xs:xss) (ys:yss) = (xs ++ ys) : mergeLevel xss yss  


      > Operadores:

            -- Dada una función, la aplica a un dato y devuelve la aplicación de la misma.
            -- Se usa para quitar paréntesis.
            ($) :: (a -> b) -> a -> b

            -- Dado un número, devuelve el siguiente del mismo. 
            -- Es una muestra que la forma del operador de la suma parcial ser así.
            (1+) :: Int -> Int

            -- Dado un número, devuelve el mismo pero restado por 10.
            -- Representa la resta (-10), el número que le des le resta 10.
            subtract 10 :: Int -> Int


> Conceptos Vistos en Funciones:

      > Variables globales que cambian en el tiempo:
            > La idea es terminar teniendo una función como resultado, donde el tipo
              suele ser (Int -> a). Es por eso que por currificación se puede escribir
              explícitamente que el resultado es una función.

            take :: Int -> [a] -> [a]
            take = flip $ foldr ctk $ const []
                  where ctk x xs' = \n -> if n == 0 then [] else x : xs' (n-1)

            drop :: Int -> [a] -> [a]
            drop = flip $ foldr ctk $ const []
                  where ctk x xs' = \n -> if n == 0 then x : xs' n else xs' (n-1)

            elemAt :: Int -> [a] -> a
            elemAt = flip $ foldr ctk $ const $ error "No existe esa posición en la lista"
                  where ctk x xs' = \n -> if n == 0 then x else xs' (n-1)

            takeV2 :: Int -> [a] -> [a]
            takeV2 = flip $ foldr ctk $ const []
                  where ctk x xs' n = if n == 0 then [] else x : xs' (n-1)
                  
            dropV2 :: Int -> [a] -> [a]
            dropV2 = flip $ foldr ctk $ const []
                  where ctk x xs' n = if n == 0 then x : xs' n else xs' (n-1)

            elemAtV2 :: Int -> [a] -> a
            elemAtV2 = flip $ foldr ctk $ const $ error "No existe esa posición en la lista"
                  where ctk x xs' n = if n == 0 then x else xs' (n-1)


      > Variables globales que no cambian en el tiempo:
            > La idea es tener de referencia un dato para operar en la recursión.
            > Como no varía en el tiempo, no hace falta actualizarlo y basta declararlo una vez.

            insertT :: a -> Tree a -> Tree a
            insertT x = recT (NodeT x EmptyT EmptyT) fNodeT
                  where fNodeT y ti ti' td td' = case (compare x y) of
                                                   EQ -> NodeT y ti  td
                                                   LT -> NodeT y ti' td
                                                   GT -> NodeT y ti  td'

            caminoHasta :: Ord a => a -> Tree a -> [a]
            caminoHasta x = recT [] fNodeT
                  where fNodeT y ti xsi td xsd = case (compare x y) of
                                                   EQ -> [y]
                                                   LT -> y : xsi
                                                   GT -> y : xsd


      > Tuppling / Algoritmos Similares:
            > La idea es tener una función que tome el resultado de otra función que realiza
              la búsqueda y suele devolver una tupla de tipo (a, b) con los datos necesarios.
            > En la función que toma el resultado simplemente se interpreta el resultado.
            > En la función que realiza la recursión se modifica la tupla con los datos necesarios.

            > Tuppling:

                  objectsOfLongestPath :: Mapa a -> [a]
                  objectsOfLongestPath m = let (ds, xs) = objectsOfLongestPath' m
                                          in xs

                  objectsOfLongestPath' :: Mapa a -> ([Dir], [a])
                  objectsOfLongestPath' = foldM fCofre fNada fBifurcacion
                        where fCofre xs                            = ([], xs);
                              fNada (ds, xs)                       = (Straight:ds, xs);
                              fBifurcacion _ (dsi, xsi) (dsd, xsd) = if length dsi >= length dsd
                                                                        then (Left : dsi, xsi)
                                                                        else (Right : dsd, xsd)

            > Algoritmos Similares:

                  losAntecesoresDe :: Eq a => Tree a -> a -> [a]
                  losAntecesoresDe t x = case (antecesoresDe x t) of
                                          Just xs -> xs
                                          Nothing -> error "El elemento no se encuentra en el árbol"

                  antecesoresDe :: Eq a => a -> Tree a -> Maybe [a]
                  antecesoresDe x EmptyT          = Nothing
                  antecesoresDe x (NodeT y n1 n2) = if x == y 
                                                      then Just [] 
                                                      else elegirRama y (antecesoresDe x n1)
                                                                        (antecesoresDe x n2)

                  elegirRama :: a -> Maybe [a] -> Maybe [a] -> Maybe [a]


      > Diferencia entre PreOrder - InOrder - PostOrder:

            preOrder :: Tree a -> [a]
            preOrder = foldT [] fNodeT
                  where fNodeT x xsi xsd = x : xsi ++ xsd

            inOrder :: Tree a -> [a]
            inOrder = foldT [] fNodeT
                  where fNodeT x xsi xsd = xsi ++ [x] ++ xsd

            postOrder :: Tree a -> [a]
            postOrder = foldT [] fNodeT
                  where fNodeT x xsi xsd = xsi ++ xsd ++ [x]


> Esquemas de Funciones:

      map :: (a -> b) -> [a] -> [b] 
      map f []     = []
      map f (x:xs) = f x : map f xs 

      filter :: (a -> Bool) -> [a] -> [a] 
      filter p []     = []
      filter p (x:xs) = if p x
                           then x : filter p xs
                           else filter p xs

      foldr :: (a -> b -> b) -> b -> [a] -> b 
      foldr f z []     = z
      foldr f z (x:xs) = f x (foldr f z xs)

      foldr1 :: (a -> a -> a) -> [a] -> a 
      foldr1 f []     = error "No existen elementos en la lista"
      foldr1 f [x]    = x
      foldr1 f (x:xs) = f x (foldr1 f xs)

      recr :: b -> (a -> [a] -> b -> b) -> [a] -> b 
      recr z f []     = z
      recr z f (x:xs) = f x xs (recr z f xs)


> Esquemas de Funciones sobre Tipos Algebraicos:

      {- Mapa:
            data Mapa a = Cofre [a]
                        | Nada (Mapa a)
                        | Bifurcacion [a] (Mapa a) (Mapa a)
      -}

      foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
      foldM fc fn fb = go
            where go (Cofre xs)             = fc xs;
                  go (Nada m)               = fn (go m);
                  go (Bifurcacion xs ml mr) = fb xs (go ml) (go mr)

      recM :: ([a] -> b) -> (Mapa a -> b -> b) -> ([a] -> Mapa a -> b -> Mapa a -> b -> b) -> Mapa a -> b
      recM fc fn fb = go
            where go (Cofre xs)             = fc xs;
                  go (Nada m)               = fn m (go m);
                  go (Bifurcacion xs mi md) = fb xs mi (go mi) md (go md)

      {- ExpA:
            data ExpA = Cte Int
                      | Suma ExpA ExpA
                      | Prod ExpA ExpA
      -}

      foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
      foldExpA f g h = go
            where go (Cte n)      = f n;
                  go (Suma e1 e2) = g (go e1) (go e2);
                  go (Prod e1 e2) = h (go e1) (go e2)

      recExpA :: (Int -> b) -> (ExpA -> b -> ExpA -> b -> b) -> (ExpA -> b -> ExpA -> b -> b) -> ExpA -> b
      recExpA f g h = go
            where go (Cte n)      = f n;
                  go (Suma e1 e2) = g e1 (recExpA f g h e1) e2 (recExpA f g h e2);
                  go (Prod e1 e2) = h e1 (recExpA f g h e1) e2 (recExpA f g h e2)

      {- AppList:
            data AppList a = Single a
                           | Append (AppList a) (AppList a)
      -}

      foldAL :: (a -> b) -> (b -> b -> b) -> AppList a -> b
      foldAL fs fa = go 
            where go (Single x)       = fs x;
                  go (Append ali ald) = fa (go ali) (go ald)

      recAL :: (a -> b) -> (AppList a -> b -> AppList a -> b -> b) -> AppList a -> b
      recAL fs fa = go
            where go (Single x)	  = fs x;
                  go (Append ali ald) = fa ali (go ali) ald (go ald)


> Significado de Términos Utilizados:

      > Recursión implícita:
            > Es cuando se utiliza un esquema de función para realizar recursión sobre una estructura.

      > Recursión explícita:
            > Es cuando se plantean los casos necesarios para realizar recursión sobre una estructura
              de forma explícita.


      > Funciones totales y parciales:
            > Las funciones totales, son aquellas que nunca dan bottom si recibe valores totalmente definidos.
            > Las funciones parciales, son aquellas que pueden dar bottom al recibir valores totalmente definidos. 

      > Funciones estrictas y no estrictas:
            > Las funciones estrictas, son aquellas que siempre devuelve bottom si recibe bottom.
            > Las funciones no estrictas, son aquellas que pueden devolver algo diferente a bottom si recibe bottom.


      > Forma de asociación en funciones y en el tipo:
            > La forma de asociación en funciones es a izquierda.
            > La forma de asociación en el tipo es a derecha.


      > Sección de operadores:
            > Es una sintaxis especial para aplicación parcial en operadores infijos.
            > La notación infijo es aquella donde el nombre se encuentra entre sus argumentos.
            > La notación prefijo es aquella donde el nombre se encuentra antes de sus argumentos.

            > Al poner entre paréntesis el operador y uno de sus argumentos devuelve una función
              intermedia que espera el segundo argumento.

            > Sección Izquierda:
                  Infijo:  (2*)
                  Prefijo: (*) 2

            > Sección Derecha:
                  Infijo:  (*2)
                  Prefijo: flip (*) 2

            > Backticks:
                  Infijo:  foldr (+) 0
                  Prefijo: (+) `foldr` 0


> Reglas para Definir Conjuntos Inductivos:

      > Reglas base:
            > Una o varias afirmaciones directas que determinan qué elemento es parte del conjunto.
            > Ejemplo: Habitacion a está en el conjunto Dungeon a.
            

      > Reglas inductivas:
            > Una o varias implicaciones (productivas) que determinan qué elemento es parte del conjunto.
            > Ejemplo: Si m está en el conjunto Maybe a, d está en el conjunto Dungeon a, entonces
                       Pasaje m d está en el conjunto Dungeon a.


      > Condición Adicional:
            > Que se pida el menor de todos los conjuntos es porque, sería totalmente válido sin la regla
              adicional que sea válido un conjunto que tenga el conjunto inductivo buscado y además otros
              más dentro del mismo, porque cumpliría las reglas.
            > La idea entonces es que SOLAMENTE tenga sentido quedarse con el que ÚNICAMENTE tenga el
              conjunto inductivo buscado y nada más: "el menor de los que cumplen".
            > Por eso esta regla provee unicidad, ya que siempre el resultado va a ser el conjunto
              inductivo buscado.


> Conceptos Vistos en Demostraciones:

      > Ley de Morgan [P6-E2-A y P6-E2-B]:
            - Para todo x. para todo y. x && y = not ((not x) || (not y))
            - Para todo x. para todo y. not (x || y) = not x && not y


      > La identidad en funciones que toman dos parámetros [P6-E4-A y P6-E4-B]:
            - Para todo f. curry (uncurry f) = f 
            - Para todo f. uncurry (curry f) = f 


      > La identidad para pares [P6-E3-III-B]:
            - swap . swap = id


      > Asociatividad de la composición [P6-E7-B-III]:
            - Para todo f. para todo g. para todo h. f . (g . h) = (f . g) . h


      > Aplicar el cambio de asociación en pares es lo mismo que componer ambas funciones [P6-E3-III-B]:
            - Para todo f. appAssoc f = f . assoc


      > Manipular simbólicamente una estructura ida y vuelta devuelve lo mismo [P8-E3-III-B]:















> Distintas formas de expresar situaciones en los planteos de las demostraciones:

      > Estructuras de demostraciones con principio de extensionalidad:
            > Versión 1 (Básica):
            - Por principio de extensionalidad, es equivalente demostrar que:
                  ¿...?

                  Sea ... Se verá que ¿...?

            > Versión 2 (Aplicando definiciones):
            - Por principio de extensionalidad, es equivalente demostrar que:
                  ¿...?

                  Por definición de ... es equivalente demostrar que:
                  ¿...?

                  Sea ... Se verá que ¿...?

            > Versión 3 (Aplicando definiciones y varias veces el principio):
            - Por principio de extensionalidad (nro_veces), es equivalente demostrar que:
                  ¿...?

                  Por definición de ... es equivalente demostrar que:
                  ¿...?

                  Sea ... Se verá que ¿...?


      > Expresiones válidas dentro del "Sea ...":
            > Si únicamente hay una variable:
            - "Sea x :: a. Se verá que ¿f x = g x?"

            > Si hay más de dos variables:
            - "Sea x :: a, sea y :: b, ... Se verá que ¿f x y = g x y?"

            > Formas de expresar los distintos tipos de variables:
            > Pares:
                  - "Sea (x, y) un par de tipo (a, b). Se verá que ¿f (x, y) = g (x, y)?"
                  - "Sea (x, y) :: (a, b). Se verá que ¿f (x, y) = g (x, y)?"
                  
                  - "Sea p un par de tipo (a, b). Se verá que ¿f p = g p?"
                  - "Sea p :: (a, b). Se verá que ¿f p = g p?"

            > Funciones:
                  - "Sea h :: (a -> b). Se verá que ¿h x = g x?"
                  - "Sea h una función de tipo (a -> b). Se verá que ¿h x = g x?"
                  - "Sea h una función cualquiera. Se verá que ¿h x = g x?"

            > Elementos de tipos polimórficos:
                  - "Sea x :: a. Se verá que ¿f x = g y?"
                  - "Sea x :: a, sea y :: b. Se verá que ¿f x = g y?"


            > Elementos con tipos instanciados:
                  - "Sea n un número. Se verá que ¿f n = g y?"
                  - "Sea n y m dos números. Se verá que ¿f n = g m?"
                  - "Sea b un booleano. Se verá que ¿f x = g y?"
