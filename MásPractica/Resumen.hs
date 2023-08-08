module Resumen where

-- Interfaz del Tipo Abstracto Resumen 🐸

-- Propósito  : Crea un resumen vacío.
-- Complejidad: O(1)
nuevoR       :: Resumen 

-- Propósito  :Agrega la observación dada al resumen dado.
-- Complejidad: O(1)
agregarR     :: Int -> Resumen -> Resumen

-- Propósito  :Describe la cantidad total de observaciones registradas en el resumen dado.
-- Complejidad: O(1)
cantidadR    :: Resumen -> Int 

-- Propósito   : Describe la observación máxima registrada en el resumen dado.
-- Precondición: Debe haber al menos una observación registrada.
-- Complejidad : O(1)
maximoR      :: Resumen -> Int 

-- Propósito   : Describe el promedio de las observaciones registradas en el resumen dado.
-- Precondición: Debe haber al menos una observación registrada.
-- Complejidad : O(1)
promedioR    :: Resumen -> Int 

-- Implementación del Tipo Abstracto Resumen 🥐 

data Resumen = MkR Int -- cantidad de observaciones  
				   Int -- máxima observación registrada
				   Int -- suma total de observaciones 

-- ▪ 
nuevoR = MkR 0 0 0 

-- ▪ 
agregarR observacion (MkR cantidad maximoAlMomento suma) = 
	MkR (cantidad+1) 
	    (maximoEntre maximoAlMomento observacion)
	    (suma+observacion)  

-- Describe el máximo entre los dos números dados.
-- 
maximoEntre :: Int -> Int -> Int 
maximoEntre x y = 
	if x >= y  
		then x  
		else y   

-- ▪ 
cantidadR (MkR cantidad _ _) = cantidad 

-- ▪
maximoR (MkR _ maximo _)   = maximo  

-- ▪ 
promedioR (MkR cantidad _ suma) = div suma cantidad 