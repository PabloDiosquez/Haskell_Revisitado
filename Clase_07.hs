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

iguales :: Eq a => Set a -> Set b -> Bool 
iguales xs cs = (incluido xs cs) && (incluido cs xs) 