-- TIPOS ALGEBRAICOS ☂ 

-- Tipos enumerativos 

data Origen = Animal | Vegetal | Mineral deriving Show

-- Definir la función nroAnimales :: [Origen] -> Int 

-- Propósito:
-- 
-- Precondiciones:
-- 
-- 
nroAnimales :: [Origen] -> Int 
nroAnimales [] = 0  
nroAnimales (origen:origenes) = 
	if esAnimal origen
		then 1 + nroAnimales origenes
		else nroAnimales origenes 

-- Propósito:
-- 
-- Precondiciones:
-- 
-- 
esAnimal :: Origen -> Bool 
esAnimal Animal = True 
esAnimal _      = False 