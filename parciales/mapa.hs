-- PARCIAL: Mapa (2018s1).

data Dir = L | R | S
data Mapa a = Cofre [a] 
            | Nada (Mapa a)
            | Bifurcacion [a] (Mapa a) (Mapa a)


-- Ejercicio 1:

objects :: Mapa a -> [a]
objects (Cofre xs)             = xs
objects (Nada m)               = objects m
objects (Bifurcacion xs ml mr) = xs ++ objects ml ++ objects mr


mapM :: (a -> b) -> Mapa a -> Mapa b
mapM f (Cofre xs)             = Cofre (map f xs)
mapM f (Nada m)               = Nada (mapM f m)
mapM f (Bifurcacion xs ml mr) = Bifurcacion (map f xs) (mapM f ml) (mapM f mr)


hasObjectAt :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt f (Cofre xs)             ds = if null ds
                                             then any f xs
                                             else error "El camino es inválido."
hasObjectAt f (Nada m)               ds = case ds of
                                             (S:ds') -> hasObjectAt f m ds'
                                             _       -> error "El camino es inválido."
hasObjectAt f (Bifurcacion xs ml mr) ds = case ds of
                                             (L:ds') -> hasObjectAt f ml ds'
                                             (R:ds') -> hasObjectAt f mr ds'
                                             []      -> any f xs
                                             _       -> error "El camino es inválido."


longestPath :: Mapa a -> [Dir]
longestPath m = fst (objectsLongestPath m)


objectsOfLongestPath :: Mapa a -> [a]
objectsOfLongestPath m = snd (objectsLongestPath m)

objectsLongestPath :: Mapa a -> ([Dir], [a])
objectsLongestPath (Cofre xs)             = ([] , xs)
objectsLongestPath (Nada m)               = let (dsM, objM) = objectsLongestPath m
                                             in ((S:dsM), objM) 
objectsLongestPath (Bifurcacion xs ml mr) = let (dsML, objML) = objectsLongestPath ml 
                                                (dsMR, objMR) = objectsLongestPath mr
                                             in if length dsML > length dsMR
                                                   then ((L:dsML), xs ++ objML)
                                                   else ((R:dsML), xs ++ objMR)


allPaths :: Mapa a -> [[Dir]]
allPaths (Cofre _)             = [[]]
allPaths (Nada m)              = map (S:) (allPaths m)
allPaths (Bifurcacion _ ml mr) = (map (L:) (allPaths ml)) ++ (map (R:) (allPaths mr)) 


-- Ejercicio 2:

foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM fc fn fb = go
    where go (Cofre xs)             = fc xs
          go (Nada m)               = fn (go m)
          go (Bifurcacion xs ml mr) = fb xs (go ml) (go mr)

recM :: ([a] -> b) -> (Mapa a -> b -> b) -> ([a] -> Mapa a -> b -> Mapa a -> b -> b) -> Mapa a -> b
recM fc fn fb = go
    where go (Cofre xs)             = fc xs
          go (Nada m)               = fn m (go m)
          go (Bifurcacion xs ml mr) = fb xs ml (go ml) mr (go mr)


-- Ejercicio 3:

