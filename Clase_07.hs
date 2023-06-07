module Clase_07
where 

type Set a = [a] 

vacio :: Set [a]
vacio = [] 

pertenece :: Eq a => a -> Set a -> Bool
pertenece _ []     = False 
pertenece x (c:cs) = x == c || pertenece x cs 


agregar :: Eq a => a -> Set a -> Set a 
agregar x cs | not (pertenece x cs) = x : cs 
			 | otherwise            = cs 


incluido :: Eq a => Set a -> Set a -> Bool 
incluido [] _      = True 
incluido (x:xs) cs = (pertenece x cs) && incluido xs cs 


iguales :: Eq a => Set a -> Set a -> Bool 
iguales xs cs = (incluido xs cs) && (incluido cs xs)


union :: Eq a => Set a -> Set a -> Set a 
union [] cs     = cs 
union (x:xs) cs = union xs (agregar x cs) 


interseccion :: Eq a => Set a -> Set a -> Set a
interseccion [] _ = [] 
interseccion (x:xs) cs | pertenece x cs = x : interseccion xs cs 
					   | otherwise      =     interseccion xs cs 


diferencia :: Eq a => Set a -> Set a -> Set a 
diferencia [] _ = []
diferencia (x:xs) cs | not (pertenece x cs) = x : diferencia xs cs 
					 | otherwise            =     diferencia xs cs 


diferenciaSimetrica :: Eq a => Set a -> Set a -> Set a 
diferenciaSimetrica xs cs = diferencia (union xs cs) (interseccion xs cs) 

-- CONJUNTO PARTES DE UN CONJUNTO ☂

partes :: Eq a => Set a -> Set (Set a)
partes []     = [[]] 
partes (x:xs) = unionC (partes xs) (agregarATodos x (partes xs))


agregarATodos :: Eq a => a -> Set (Set a) -> Set (Set a)
agregarATodos x []       = []
agregarATodos x (cs:css) = agregarC (agregar x cs) (agregarATodos x css) 

-- FUNCIONES AUXILIARES 🐱‍🏍

unionC :: Eq a => Set (Set a) -> Set (Set a) -> Set (Set a)
unionC [] css       = css 
unionC (xs:xss) css = unionC xss (agregarC xs css)


agregarC :: Eq a => Set a -> Set (Set a) -> Set (Set a)
agregarC xs css | perteneceC xs css =      css 
				| otherwise         = xs : css

perteneceC :: Eq a => Set a -> Set (Set a) -> Bool
perteneceC xs []       = False 
perteneceC xs (cs:css) = (iguales xs cs) || perteneceC xs css  