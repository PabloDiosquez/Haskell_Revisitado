module Gramola where 

import Resumen 

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

-- Propósito  : Describe el nombre de la canción más votada en la gramola dada. Si nunca fue votado, 
-- describe 0.
-- Complejidad: O(1)
--
temaMasVotadoG :: Gramola -> String 

-------------------------------------------------------------------------------------------------
-- Implementación del tipo abstracto de datos Gramola ☂ 

data Gramola = G [(String, Resumen)]	-- canciones anteriores con sus votos (en el orden inverso).
				 String 			    -- canción actual. 
				 Resumen 				-- votos del tema actual. 
				 [(String, Resumen)]	-- canciones posteriores con sus votos. 
				 String 				-- tema más votado.
				 Int 					-- cantidad de votos del tema más votado. 

-- INVARIANTE de representación: 
-- El nombre del tema más votado es alguno de los nombres de temas de la gramola y es el que 
-- tiene mayor cantidad de votos.
--
-- El número que representa la cantidad de votos del tema más votado debe ser verdaderamente 
-- la cantidad de votos que recibió el tema más votado de toda la gramola.
-- 

-- ▪ 
nuevaG (cancion : canciones) = 
	G [] cancion nuevoR (cancionesConResumenesVacios canciones) cancion 0

cancionesConResumenesVacios :: [String] -> [(String, Resumen)]
cancionesConResumenesVacios [] 					  = [] 
cancionesConResumenesVacios (cancion : canciones) = 
	(cancion, nuevoR) : cancionesConResumenesVacios canciones

-- ▪ 
anteriorG (G [] act r sigs tmv ptmv) 				 =
		   G [] act r sigs tmv ptmv
anteriorG (G ((ant, rant):ants) act r sigs tmv ptmv) = 
		   G  ants ant rant ((act, r):sigs) tmv ptmv

-- ▪
siguienteG (G ants act r [] tmv ptmv) 			  = 
			G ants act r [] tmv ptmv
siguienteG (G ants act r ((s, rs):sigs) tmv ptmv) = 
			G ((act, r) : ants) s rs sigs tmv ptmv   

-- ▪
cancionG (G _ c _) = c 

-- ▪ 
votarG puntaje (G ants act r sigs tmv ptmv) = 
	if cantidadR r == ptmv
		then G ants act (agregarR puntaje r) act (ptmv + 1)
		else G ants act (agregarR puntaje r) sigs tmv ptmv

-- ▪ 
puntajeG (G ants act r sigs tmv ptmv) = 
	if cantidadR r == 0 
		then 0 
		else promedioR r 

-- ▪ 
temaMasVotadoG (G _ _ _ tmv _) = tmv 