> Funciones Útiles para el Parcial:

      compare :: Ord a => a -> a -> Ordering 
      compare x y = if x == y
                       then EQ
                       else if x >= y
                               then GT
                               else LT

      {- (No hace falta definirla)
            caminoHasta :: Ord a => a -> Tree a -> [a]
            caminoHasta x = recT [] fNodeT
                  where fNodeT y ti xsi td xsd = case (compare x y) of
                                                   EQ -> [y]
                                                   LT -> y : xsi
                                                   GT -> y : xsd
      -}

      maybe :: a -> (b -> a) -> Maybe b -> a
      maybe _ f (Just x) = f x
      maybe z _ Nothing  = z

      {- (Hay que definirla)
            findAtAndTransform :: a -> (b -> a) -> b -> Tree b -> a 
            findAtAndTransform z f x t = maybe z f (findAt x t)

            findAt :: a -> Tree a -> Maybe a
      -}

      transformMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
      transformMaybe f (Just x) my = Just (maybe x (f x) my)
      transformMaybe _ Nothing  my = my

      {- (Hay que definirla)
            cantidadAntecesores :: Eq a => a -> Tree a -> Maybe Int 
            cantidadAntecesores z EmptyT          = Nothing
            cantidadAntecesores z (NodeT x ti td) = 
                  if x == z
                     then Just 1
                     else transformMaybe (+) (cantidadAntecesores z ti) (cantidadAntecesores z td) 
      -}

      elegirEntre :: Maybe a -> Maybe a -> Maybe a
      elegirEntre (Just x) m = Just x
      elegirEntre Nothing  m = m

      {- (Hay que definirla)
            findAt :: Eq a => a -> Tree a -> Maybe a
            findAt z EmptyT          = Nothing
            findAt z (NodeT x ti td) = if x == z
							then Just x
							else elegirEntre (findAt z ti) (findAt z td)

            elegirEntre :: Maybe a -> Maybe a -> Maybe a
            elegirEntre (Just x) m = Just x
            elegirEntre Nothing  m = m
      -}

      agregarA :: a -> Maybe [a] -> Maybe [a]
      agregarA y Nothing   = Nothing
      agregarA y (Just xs) = Just (y:xs)

      {- (Hay que definirla)
            filterBy :: (a -> Bool) -> [a] -> Maybe [a]
            filterBy f []     = Nothing
            filterBy f (x:xs) = if f x
                                   then agregarA x (filterBy f xs)
                                   else filterBy f xs
      -}


> Funciones Fundamentales:

      first :: (a, b) -> a
      first (x, _) = x 

      second :: (a, b) -> b
      second (_, y) = y

      swap :: (a, b) -> (b, a)
      swap (x, y) = (y, x)

      const :: a -> b -> a
      const x y = x

      id :: a -> a
      id x = x

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

      appPar :: (a -> b , c -> d) -> ((a , c) -> (b , d))
      appPar (f, g) (x, y) = (f x, g y)

      appDist :: (a -> b) -> (a , a) -> (b , b)
      appDist f (x, y) = (f x, f y)

      assoc :: (a, (b, c)) -> ((a, b), c)
      assoc (x, (y, z)) = ((x, y), z)

      appAssoc :: (((a, b), c) -> d) -> (a, (b, c)) -> d
      appAssoc f p = f (assoc p)

      scanr :: (a -> b -> b) -> b -> [a] -> [b]
      scanr _ z []     = [z]
      scanr f z (x:xs) = let (y:ys) = scanr f z xs
                         in f x y : (y:ys)


> Conceptos Vistos en Funciones:

      > Variables globales que cambian en el tiempo:

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




      > Tuppling:

      losAntecesoresDe :: Eq a => Tree a -> a -> [a]
      losAntecesoresDe t x = case (buscarAntecesoresSiExiste x t) of
            Just xs -> xs
            Nothing ->  error "El elemento no se encuentra en el árbol"

      buscarAntecesoresSiExiste :: Eq a => a -> Tree a -> Maybe [a]
      buscarAntecesoresSiExiste x EmptyT          = Nothing
      buscarAntecesoresSiExiste x (NodeT e n1 n2) = if (x == e) 
                                                       then Just [] 
                                                       else elegirRama e (buscarAntecesoresSiExiste x n1)
                                                                         (buscarAntecesoresSiExiste x n2)

      elegirRama :: a -> Maybe [a] -> Maybe [a] -> Maybe [a]
      elegirRama e (Just xs) _ = Just (e:xs)
      elegirRama e _ (Just ys) = Just (e:ys)
      elegirRama e _         _ = Nothing 

      longestPathFold :: Mapa a -> [Dir]
      longestPathFold = fst . caminoMasLargoYObjetosFold            

      objectsOfLongestPathFold :: Mapa a-> [a]
      objectsOfLongestPathFold = snd . caminoMasLargoYObjetosFold

      caminoMasLargoYObjetosFold :: Mapa a -> ([Dir],[a])
      caminoMasLargoYObjetosFold = foldMapa caminoLargoYObjetosCOFRE caminoLargoYObjetosNADA caminoLargoYObjetosBIFUR
                  where caminoLargoYObjetosCOFRE xs = ([],xs)
                        caminoLargoYObjetosNADA (ds,objs) = (Straight:ds,objs)
                        caminoLargoYObjetosBIFUR xs (ds1,obj1) (ds2,obj2) =
                                          if length ds1 > length ds2
                                          then (Left:ds1, xs ++ obj1)
                                          else (Right:ds2, xs ++ obj2)                                                                      

      hasObjectAtFold :: (a -> Bool) -> Mapa a -> ([Dir] -> Bool)
      hasObjectAtFold f =  foldMapa hasObjectAtFoldCOFRE hasObjectAtFoldNADA hasObjectAtFoldBIFUR
                  where hasObjectAtFoldCOFRE xs [] = any f xs
                        hasObjectAtFoldCOFRE xs _  = error "camino invalido"

                        hasObjectAtFoldNADA h (Straight:ds) = h ds
                        hasObjectAtFoldNADA h _            = error "camino invalido"

                        hasObjectAtFoldBIFUR xs h1 h2 []           = any f xs
                        hasObjectAtFoldBIFUR xs h1 h2 (Left: ds)   = h1 ds
                        hasObjectAtFoldBIFUR xs h1 h2 (Right: ds)  = h2 ds
                        hasObjectAtFoldBIFUR xs h1 h2 _            = error "camino invalido"


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
                      | Prod ExpA Exp
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

      {- QuadTree:
            data QuadTree a = LeafQ a
                            | NodeQ (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
      -}

      foldQT :: (a -> b) -> (b -> b -> b -> b -> b) -> QuadTree a -> b
      foldQT fl fn = go
            where go (LeafQ x)           = fl x;
                  go (NodeQ q1 q2 q3 q4) = fn (go q1) (go q2) (go q3) (go q4)

      recQT :: (a -> b) -> (QuadTree a -> b -> QuadTree a -> b -> QuadTree a -> b -> QuadTree a -> b -> b) -> QuadTree a -> b
      recQT fl fn = go
            where go (LeafQ x)           = fl x;
                  go (NodeQ q1 q2 q3 q4) = fn q1 (go q1) q2 (go q2) q3 (go q3) q4 (go q4)


> Significado de Términos Utilizados:

      > Recursión implícita:


      > Recursión explícita:


      > :







> Conceptos Vistos en Demostraciones:

      > A




      > A




      > A




      > A




      > A




      > A




      > A




      > A




      > A

