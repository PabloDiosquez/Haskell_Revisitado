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

-- Tipos productos (registros)🐣 -- Los constructores pueden tener parámetros -- 

-- Un ingrediente cuenta con una descripción, su origen y un valor energético por cada 100gr
-- expresados en calorías.

type Nombre = String -- renombre de tipos 

-- Propósito:
-- Modela un ingrediente.
-- Inv. Rep.:
-- * El nombre del ingrediente no debe ser vacío.
-- * El valor energético debe ser un número >= 0.

data Ingrediente = MKI Nombre Origen Int 
	deriving Show

bollo :: Ingrediente
bollo = MKI "Masa de pizza" Vegetal 228

tomate :: Ingrediente
tomate = MKI "Tomate" Vegetal 22 

queso :: Ingrediente
queso = MKI "Queso muzzarella" Animal 245  

-- Funciones observadoras (proyectoras) 👁‍🗨

-- Propósito:
-- Describe el valor del primer campo (descripción) del ingrediente dado.
-- Precondiciones:
-- No tiene (es una función total).
-- 
descripcionI :: Ingrediente -> Nombre 
descripcionI (MKI nombre _ _) = nombre 

-- Propósito:
-- Describe el origen del ingrediente dado.
-- Precondiciones:
-- No tiene (es una función total).
-- 
origenI :: Ingrediente -> Origen 
origenI (MKI _ origen _) = origen 

-- Propósito:
-- Describe el valor energético del ingrediente dado.
-- Precondiciones:
-- No tiene (es una función total).
-- 
valorEnergeticoI :: Ingrediente -> Int 
valorEnergeticoI (MKI _ _ valorEnergetico) = valorEnergetico 