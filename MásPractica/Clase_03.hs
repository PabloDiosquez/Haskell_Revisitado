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

---------------------------------
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

-- Tipos suma 🥨

data Fuego = Fuerte | Moderado | Suave 
	deriving Show

data MetodoCoccion = Horno Fuego Int 
				   | Hervido
				   | Frito 
				   deriving Show

-- Definir la función demoraMC :: MetodoCoccion -> Int suponiendo que el segundo parámetro de Horno
-- representa una cantidad de minutos de horno, mientras que hervir demora 30 minutos y freír demora
-- 15 minutos.

demoraMC :: MetodoCoccion -> Int 
demoraMC (Horno fuego minutos) = minutos 
demoraMC Hervido 		       = 30 
demoraMC Frito   		       = 15

-----------------------------------
-- Tipos recursivos 👽

-- El siguiente tipo de datos sirve para reprensetar recetas. El primer
-- parámetro de Agregar representa el peso en gramos.

data Receta = Empezar
		    | Agregar Int Ingrediente Receta 
		    | Cocinar MetodoCoccion Receta 
		    deriving Show

-- Representar la siguiente receta para una pizza:

-- "Estirar el bollo previamente preparado (200g) sobre una pizzera
-- desde el centro hacia los bordes. Pincelar con puré de tomates (150g).
-- Llevar a horno fuerte durante 10 minutos. Agregar queso muzzarella rallado (100g).
-- Llevar a horno moderado durante 20 minutos."

recetaParaPizza :: Receta 
recetaParaPizza =
		Cocinar (Horno Moderado 20)
		(Agregar 100 queso 
		(Cocinar (Horno Fuerte 10)
		(Agregar 150 tomate 
		(Agregar 200 bollo Empezar))))

-- Algunas funciones sobre recetas 😻

-- 1️⃣
-- Propósito:
-- Describe la cantidad de calorías que aporta en total la comida terminada, teniendo en cuenta
-- el valor energético y la cantidad incorporada de cada ingrediente.
-- Precondiciones:
-- No tiene (es una función total).
--
kCalTotalesR :: Receta -> Int
kCalTotalesR Empezar = 0 
kCalTotalesR (Agregar gramos ingrediente receta) =
	(kCalTotalesI gramos ingrediente) + kCalTotalesR receta
kCalTotalesR (Cocinar _ receta) = kCalTotalesR receta  
			
-- Propósito:
-- Describe la cantidad de calorías que aporta la comida dada, teniendo en cuenta el valor 
-- energético y la cantidad de gramos incorporada.
-- Precondiciones:
-- **gramos** debe ser >= 0.
--
kCalTotalesI :: Int -> Ingrediente -> Int
kCalTotalesI gramos (MKI _ _ valorEnergetico) = div (gramos * valorEnergetico) 100

-- 2️⃣  
-- Propósito:
-- Describe el tiempo que demora en total la cocción, despreciando el tiempo de incorporar 
-- los ingredientes.
-- Precondiciones:
-- No tiene (es una función total).
--
tiempoCoccionR :: Receta -> Int 
tiempoCoccionR Empezar                        = 0  
tiempoCoccionR (Agregar _ _ receta)           = tiempoCoccionR receta   
tiempoCoccionR (Cocinar metodoCoccion receta) = 
	(demoraMC metodoCoccion) + tiempoCoccionR receta  