module Gramola where 

-- Interfaz del Tipo Abstracto Gramola 🤘

-- Propósito   : Crea una nueva gramola.
-- Precondición: La lista dada no debe ser vacía. 
-- Complejidad : O(1)
--
nuevaG 	       :: [String] -> Gramola  

-- Propósito  : Retrocede a la canción anterior en la gramola dada. 
-- Complejidad: O(1)
--
anteriorG      :: Gramola -> Gramola 

-- Propósito  : Avanza a la siguiente canción en la gramola dada. 
-- Complejidad: O(1)
--
siguienteG     :: Gramola -> Gramola

-- Propósito  : Describe el nombre de la canción sobre la cual está posicionada actualmente
-- la gramola dada.
-- Complejidad: O(1)
--
cancionG       :: Gramola -> String 

-- Propósito   : Permite votar por la canción actual sobre la cual está posicionada actualmente
-- la gramola dada.
-- Precondición: El puntaje dado debe estar comprendido entre 1 y 10 inclusive.   
-- Complejidad : O(1)
--
votarG         :: Int -> Gramola -> Gramola

-- Propósito  :  Describe el puntaje en promedio de la canción sobre la cual está posicionada actualmente
-- la gramola dada.
-- Complejidad: O(1)
--
puntajeG   	   :: Gramola -> Int 

-- Propósito  : Describe el nombre de la canción más votada en la gramola dada.
-- Complejidad: O(1)
--
temaMasVotadoG :: Gramola -> String 