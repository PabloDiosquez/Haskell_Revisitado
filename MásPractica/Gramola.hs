module Gramola where 

-- Interfaz del Tipo Abstracto Gramola 🤘

-- Propósito  :  
-- Complejidad: O(1)
--
nuevaG 	       :: [String] -> Gramola  

-- Propósito  :  
-- Complejidad: O(1)
--
anteriorG      :: Gramola -> Gramola 

-- Propósito  :  
-- Complejidad: O(1)
--
seguienteG     :: Gramola -> Gramola

-- Propósito  :  
-- Complejidad: O(1)
--
cancionG       :: Gramola -> String 

-- Propósito  :  
-- Complejidad: O(1)
--
votarG         :: Int -> Gramola -> Gramola

-- Propósito  :  
-- Complejidad: O(1)
--
puntajeG   	   :: Gramola -> Int 

-- Propósito  :  
-- Complejidad: O(1)
--
temaMasVotadoG :: Gramola -> String 