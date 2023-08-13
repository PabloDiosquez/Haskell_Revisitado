module Bib where 

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
libro 		   :: Libro -> Bib
			-- O(1)

-- Propósito: 	 Describe la biblioteca que tiene todos los libros de las dos bibliotecas dadas.
-- Precondición: Las bibliotecas no deben tener libros en común.
-- 
juntar 		  :: Bib -> Bib -> Bib 
			-- No importa la eficiencia

-- Propósito:    Pide un libro prestado.
-- Precondición: El libro pedido debe existir en la biblioteca.
--
pedirPrestado :: Bib -> Fecha -> Libro -> Bib 
			-- O(lon F + log n)

-- Propósito:	Describe una biblioteca en la que el libro dado ya fue devuelto.
-- Precondicón: El libro dado debe estar prestado.
--
devolver 	  :: Bib -> Fecha -> Libro -> Bib 
			-- O(log n + log F)

-- Propósito:	Describe una lista con todos los libros de la biblioteca dada.
-- Precondicón: No tiene.
--
libros 		  :: Bib -> [Libros]
			-- O(1)

-- Propósito:	Describe el estado del libro dado en la biblioteca dada.
-- Precondición: El libro debe existir en la biblioteca dada.
--
consulta 	  :: Bib -> Libro -> ResultadoConsulta
			-- O(log n)

-- Propósito:	Indica si el libro dado fue manipulado (prestado o devuelto) en la fecha dada.
-- Precondicón: El libro debe existir en la biblioteca dada.
--
fueManipulado :: Bib -> Fecha -> Libro -> Bool 
			-- O(log n + log F)

-- ▪
-- Implementación del tipo abstracto de datos Bib

data Bib = B [Libro] 	       		-- Lista de libros de la biblioteca 📚
			 (Set Libro) 	   		-- disponibles
			 (Map Libro Fecha) 		-- prestados 
			 (Set (Libro, Fecha))	-- manipulaciones 