module Bib where 

import Set  
import Map 

-- Queremos modelar una biblioteca. Usaremos códigos numéricos para representar libros y fechas.
-- Al consultar por un libro se pueden obtener los tres resultados del tipo
-- ResultadoConsulta.

type Libro = Int 
type Fecha = Int 

data ResultadoConsulta = Inexistente
					   | Disponible 
					   | Prestado Fecha 

-- Interfaz del tipo abstracto de datos Bib

-- Propósito:    Crea una biblioteca con un único libro.
--
libroB 		    :: Libro -> Bib
			-- O(1)

-- Propósito: 	 Describe la biblioteca que tiene todos los libros de las dos bibliotecas dadas.
-- Precondición: Las bibliotecas no deben tener libros en común.
-- 
juntarB 	    :: Bib -> Bib -> Bib 
			-- No importa la eficiencia

-- Propósito:    Pide un libro prestado.
-- Precondición: El libro pedido debe existir en la biblioteca.
--
pedirPrestadoB  :: Bib -> Fecha -> Libro -> Bib 
			-- O(lon F + log n)

-- Propósito:	Describe una biblioteca en la que el libro dado ya fue devuelto.
-- Precondicón: El libro dado debe estar prestado.
--
devolverB 	    :: Bib -> Fecha -> Libro -> Bib 
			-- O(log n + log F)

-- Propósito:	Describe una lista con todos los libros de la biblioteca dada.
-- Precondicón: No tiene.
--
librosB 	    :: Bib -> [Libros]
			-- O(1)

-- Propósito:	Describe el estado del libro dado en la biblioteca dada.
-- Precondición: El libro debe existir en la biblioteca dada.
--
consultaB 	   :: Bib -> Libro -> ResultadoConsulta
			-- O(log n)

-- Propósito:	Indica si el libro dado fue manipulado (prestado o devuelto) en la fecha dada.
-- Precondicón: El libro debe existir en la biblioteca dada.
--
fueManipuladoB :: Bib -> Fecha -> Libro -> Bool 
			-- O(log n + log F)

-- ▪
-- Implementación del tipo abstracto de datos Bib

data Bib = B [Libro] 	       		-- Lista de libros de la biblioteca 📚
			 (Set Libro) 	   		-- disponibles
			 (Map Libro Fecha) 		-- prestados 
			 (Set (Libro, Fecha))	-- manipulaciones 

-- ▪
libroB libro = B [libro]
				 (addS libro emptyS) 
				 emptyM
				 emptyS
-- ▪  
-- Precondición: La lista debe ser una lista de claves del primer map.
--
juntarB (B librosA disponiblesA prestadosA manipulacionesA) 
	    (B librosB disponiblesB prestadosB manipulacionesB) =  
	     B (librosA ++ librosB)
	       (unionS disponiblesA disponiblesB)
	       (agregarPrestados (keysM prestadosA) prestadosA prestadosB)
	       (unionS manipulacionesA manipulacionesB)

-- ◽ 
agregarPrestados :: [Libro] -> Map Libro Fecha
							-> Map Libro Fecha 
							-> Map Libro Fecha 
agregarPrestados [] m1 m2 	    = m2 
agregarPrestados (l : ls) m1 m2 = 
	insertM l 
			(fromJust (lookupM l m1))
			(agregarPrestados ls m1 m2)  

-- Precondición: 
-- El valor no puede ser Nothing.
fromJust :: Maybe a -> a   
fromJust (Just f) = f 

-- ▪ 
pedirPrestadoB (B libros disponibles prestados manipulaciones) fecha libro = 
	B libros 
	  (removeS libro disponibles)
	  (insertM libro fecha prestados)
	  (addS (libro, fecha) manipulaciones)
			  

-- El TAD Conjunto ("Set") -- Interfaz 🐱‍🚀 

-- emptyS 			:: Set a 
-- addS   			:: Eq a => a -> Set a -> Set a 			-- O(log n)
-- elemS  			:: Eq a => a -> Set a -> Bool  			-- O(log n)
-- sizeS  			:: Set a -> Int 
-- removeS 			:: Eq a => a -> Set a -> Set a 			-- O(log n)
-- unionS 			:: Eq a => Set a -> Set a -> Set a 
-- intersectionS 	:: Eq a => Set -> Set a -> Set a 
-- setToListS 		:: Set a -> [a] 

-- El TAD Diccionario ("Map") -- Interfaz 🐣

-- emptyM 			:: Map a b -- O(1)
-- inserM 			:: Eq a => a -> b -> Map a b -> Map a b -- O(log n)
-- lookupM 			:: Eq a => a -> Map a b -> Maybe b      -- O(log n)
-- removeM 			:: Eq a => a -> Map a b -> Map a b  	-- O(log n)
-- keysM 			:: Map a b -> [a]  