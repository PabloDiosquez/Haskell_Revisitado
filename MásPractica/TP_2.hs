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