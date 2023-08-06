module Conjunto where 

-- Interfaz del módulo Conjunto 

-- Describe el conjunto vacío.
vacioC 	   :: Conjunto a 

-- Describe un conjunto nuevo el cual resulta de agregar el elemento dado en el conjunto dado, siempre que 
-- este elemento no esté repetido.
agregarC   :: Eq a => a -> Conjunto a -> Conjunto a 

-- Indica si el elemento dado pertenece al conjunto dado.
perteneceC :: Eq a => a -> Conjunto a -> Bool 

-- Describe la cantidad de elementos del conjunto dado.
cantidadC  :: Eq a => Conjunto a -> Int 

-- Describe un nuevo conjunto el cual resulta de borrar todos los elementos iguales al elemento dado.
borrarC    :: Eq a => a -> Conjunto a -> Conjunto a 

-- Describe la unión de los dos conjuntos dados.
unionC     :: Eq a => Conjunto a -> Conjunto a -> Conjunto a 

-- Describe una lista con los elementos del conjunto dado.
listaC     :: Eq a => Conjunto a -> [a] 


-- 1. Como usuario del tipo abstracto Conjunto implementar las siguientes funciones:
-- ▪
-- Dada una lista de elementos y un conjunto describe una lista con todos los elementos que están 
-- en el conjunto. 
losQuePertenecen :: Eq a => [a] -> Conjunto a -> [a]  
losQuePertenecen [] _     = [] 
losQuePertenecen (x:xs) c = losQuePertenecen xs (agregarC x c)

-- ▪
-- Dada una lista describe una nueva lista que resulta de quitar todos los elementos repetidos 
-- de la primera lista dada, utilizando un conjunto como estructura auxiliar.
sinRepetidos :: [a] -> [a] 
sinRepetidos xs = sinRepetidos' xs vacioC

sinRepetidos' :: Eq a => [a] -> Conjunto a -> [a]
sinRepetidos' [] c     = listaC c  
sinRepetidos' (x:xs) c = 
	sinRepetidos' xs (agregarC x c)   

-- ▪
-- Dado un árbol de conjuntos describe un conjunto formado por la unión de todos los conjuntos 
-- del árbol dado.
unirTodos :: Eq a => Arbol (Conjunto a) -> Conjunto a 
unirTodos Empty 			=  vacioC 
unirTodos (NodeT c izq der) =
	unionC c (unionC (unirTodos izq) (unirTodos der)) 

-- ▪
-- Dados dos árboles describe un conjunto con los elementos que ambos árboles tienen en común.
interseccionArbol :: Eq a => Arbol a -> Arbol a -> Conjunto a 
interseccionArbol Empty arbol 			  = vacioC  
interseccionArbol (NodeT a izq der) arbol = 
	if perteneceC a arbol 
		then agregarC a (unionC (interseccionArbol izq arbol) (interseccionArbol der arbol))
		else unionC (interseccionArbol izq arbol) (interseccionArbol der arbol)