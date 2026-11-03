--------------------------------------------------------------------------------------------------------

--                              TRABAJO PRÁCTICO N°12: Esquemas de Funciones II                       --
--                                         Conceptos Trabajados                                       --

--------------------------------------------------------------------------------------------------------

> 

first :: (a, b) -> a
first (x, _) = x 


second :: (a, b) -> b
second (_, y) = y


swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)


const :: a -> b -> a
const x y = x


suma :: Int -> Int -> Int
suma n m = n + m


doble :: Int -> Int
doble x = x + x


cuadruple :: Int -> Int
cuadruple x = x * 4


twice :: (a -> a) -> a -> a
twice f x = f (f x)


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


appAssoc :: (((a, b), c) -> d) -> (a, (b, c)) -> d
appAssoc f p = f (assoc p)


assoc :: (a, (b, c)) -> ((a, b), c)
assoc (x, (y, z)) = ((x, y), z)


flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x


uflip :: ((a, b) -> c) -> (b, a) -> c
uflip f (x, y) = f (y, x)


subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst f g x = (f x) (g x)


many :: Int -> (a -> a) -> a -> a 
many 0 f = id
many n f = f . (many (n-1) f)


compose :: (a -> b) -> (c -> a) -> c -> b
compose f g x = f (g x)


compose12 :: (d -> c) -> (a -> b -> d) -> a -> b -> c
compose12 f g x y = f (g x y)


curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)


uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y


suma' :: (Int, Int) -> Int 
suma' (x, y) = x + y


dup :: a -> (a, a)
dup x = (x, x)