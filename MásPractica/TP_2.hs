-- 1.
data Dir = Norte 
		 | Este
		 | Sur
		 | Oeste
	       deriving Show

-- Propósito:
-- ▪ Describe la dirección opuesta de la dirección dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
opuesto :: Dir -> Dir 
opuesto Norte = Sur 
opuesto Este  = Oeste
opuesto Sur   = Norte 
opuesto Oeste = Este 

-- Propósito:
-- ▪ Describe la dirección siguiente de la dirección dada, en sentido horario.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
siguiente :: Dir -> Dir 
siguiente Norte = Este 
siguiente Este  = Sur 
siguiente Sur   = Oeste
siguiente Oeste = Norte 

-- 2.
data Persona = MKPersona Nombre Edad 

type Nombre = String 
type Edad   = Int

-- Función constructora 👷🏼‍
-- Propósito:
-- ▪ Describe una persona con el nombre dado y edad = a 0.
-- Precondiciones:
-- ▪ El nombre dado no debe ser vacío.
--
nacer :: Nombre -> Persona
nacer nombre = MKPersona nombre 0 

-- Propósito:
-- ▪ Describe el nombre de la persona dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
nombre :: Persona -> Nombre
nombre (MKPersona nombre _)= nombre 

-- Propósito:
-- ▪ Describe la edad de la persona dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
edad :: Persona -> Edad
edad (MKPersona _ edad) = edad 

-- Propósito:
-- ▪ Describe a la persona dada con su edad aumentada en 1.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
crecer :: Persona -> Persona
crecer (MKPersona nombre edad) = MKPersona nombre (edad + 1)

-- Propósito:
-- ▪ Describe a la persona dada con un nuevo nombre.
-- Precondiciones:
-- ▪ "nombreNuevo" no debe ser vacío.
--
cambioDeNombre :: Nombre -> Persona -> Persona
cambioDeNombre nombreNuevo (MKPersona nombre edad) = MKPersona nombreNuevo edad 

-- Propósito:
-- ▪ Indica si la primer persona dada es menor que la segunda persona dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
esMenorQueLaOtra :: Persona -> Persona -> Bool 
esMenorQueLaOtra (MKPersona _ edad1) (MKPersona _ edad2) = edad1 < edad2

-- Propósito:
-- ▪ Describe la lista de las personas de la lista de personas dada cuyas edades superan
--   a la edad dada.
-- Precondiciones:
-- ▪ "edadASuperar" debe ser >= 0.
--
mayoresA :: Edad -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA edadASuperar (persona:personas) = 
	if (edad persona > edadASuperar)
	then persona : mayoresA edadASuperar personas
	else mayoresA edadASuperar personas

-- Propósito:
-- ▪ Describe el promedio de las edades de las personas de la lista dada.
-- Precondiciones:
-- ▪ La lista posee al menos una persona.
--
promedioEdad :: [Persona] -> Int 
promedioEdad personas = div (sumatoria (edades personas)) (longitud personas) 

edades :: [Persona] -> [Edad]
edades [] = []
edades (persona:personas) = edad persona : edades personas

-- Propósito:
-- ▪ Describe a la persona más vieja de la lista dada.
-- Precondiciones:
-- ▪ La lista posee al menos una persona.
--
elMasViejo :: [Persona] -> Persona
elMasViejo (persona:personas) = elMasViejoEntre persona personas

-- Propósito:
-- ▪ Describe al más viejo entre la persona dada y las personas de la lista dada.
--   En caso de que la lista sea vacía, describe a la primer persona dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
elMasViejoEntre :: Persona -> [Persona] -> Persona 
elMasViejoEntre elMasViejoAlMomento []                 = elMasViejoAlMomento
elMasViejoEntre elMasViejoAlMomento (persona:personas) =
						 				if esMenorQueLaOtra persona elMasViejoAlMomento
						 				then elMasViejoEntre elMasViejoAlMomento personas
						 				else elMasViejoEntre persona personas 


-- Funciones Auxiliares 🐱‍🏍 

-- Propósito:
-- ▪ Describe la longitud de la lista dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs 

-- Propósito:
-- ▪ Describe la sumatoria de los números de la lista dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
sumatoria :: [Int] -> Int 
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs 