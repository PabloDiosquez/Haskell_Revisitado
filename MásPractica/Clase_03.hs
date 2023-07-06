-- TIPOS ALGEBRAICOS ☂ 

-- Tipos enumerativos 

data Origen = Animal | Vegetal | Mineral 
			  deriving Show

-- Definir la función nroAnimales :: [Origen] -> Int 

-- Propósito:
-- Describe la cantidad de orígenes Animal que hay en la lista de orígenes dada.
-- Precondiciones:
-- No tiene (es una función total).
-- 
nroAnimales :: [Origen] -> Int 
nroAnimales [] = 0  
nroAnimales (o:os) = 
	if esAnimal o
		then 1 + nroAnimales os
		else     nroAnimales os  

-- Propósito:
-- Indica si el origen dado es Animal.
-- Precondiciones:
-- No tiene (es una función total).
-- 
esAnimal :: Origen -> Bool 
esAnimal Animal = True 
esAnimal _      = False 