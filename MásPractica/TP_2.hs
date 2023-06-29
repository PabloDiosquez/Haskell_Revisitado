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