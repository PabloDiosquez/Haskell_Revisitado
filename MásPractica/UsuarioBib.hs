module UsuarioBib where 

import Bib 

-- Ejemplo
juntar (libro 5)
	   (juntar (juntar (libro 12) (libro 9))
	          (juntar (libro 21) (libro 19)))

-- Costo de devolverTodos: 
-- O(n * (log n + log n + log F)) = 
-- O(n * (log n + log F))

devolverTodos :: Fecha -> Bib -> Bib 
devolverTodos fechaActual biblioteca = 
	devolverListaDeLibros (libros biblioteca) -- O(1)
					      fechaActual
					      biblioteca

devolverListaDeLibros :: [Libro] -> Fecha -> Bib -> Bib
devolverListaDeLibros [] fecha biblioteca 			  = biblioteca
devolverListaDeLibros (libro:libros) fecha biblioteca = 
	if estaPrestado libro biblioteca -- O(log n)
		then devolver (devolverTodos libros fecha biblioteca) fecha libro -- O(log n + log F)
		else devolverListaDeLibros libros fecha biblioteca

estaPrestado :: Libro -> Bib -> Bool 
estaPrestado bib libro = 
	case consulta bib libro of -- O(log n)
		Inexistente -> False 
		Disponible  -> False 
		Prestado f  -> True 