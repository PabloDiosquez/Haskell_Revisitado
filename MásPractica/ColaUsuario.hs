module Queue where 

-- Una Queue es un TAD de naturaleza FIFO (first in, first out). Esto significa que 
-- los elementos salen en el orden en el que entraron, es decir, el que se agrega 
-- primero es el primero en salir.

-- Interfaz del módulo Queue 🌻

-- Crea una cola vacía.
emptyQ :: Queue a 

-- Indica si la queue dada es vacía.
isEmptyQ :: Queue a -> Bool 

-- Dado un elemento y una cola agrega ese elemento a la cola.
queue :: a -> Queue a -> Queue a 

-- Describe el primer elemento de la queue dada.
-- Pre: La queue dada no debe ser vacía.
firstQ :: Queue a -> a 

-- Describe la queue dada sin su primer elemento.
-- Pre: La queue dada no debe ser vacía.
dequeue :: Queue a -> Queue a 

-- 1.
-- ▪
-- Describe la cantidad de elementos de la queue.
largoQ :: Queue a -> Int 
largoQ q = 
	if isEmptyQ q 
		then 0 
		else 1 + largoQ (dequeue q)

-- ▪
-- Dada una cola de personas, describe una lista de las mismas donde el orden de la lista es el de la cola.
atender :: Queue Persona -> [Persona]
atender ps = 
	if isEmptyQ ps 
		then []
		else firstQ ps : atender (dequeue ps) 

-- ▪
-- Inserta todos los elementos de la segunda cola en la primera.
unirQ :: Queue a -> Queue a -> Queue a
unirQ q1 emptyQ = q1  
unirQ q1 q2     = unirQ (queue (firstQ q2) q1) (dequeue q2)

-- 2. 
-- Implemente el tipo abstracto Queue utilizando listas. Los elementos deben encolarse por el final de la lista
-- y desencolarse por delante.

data Queue a = MkQ [a] 

-- ▪ 
emptyQ = MkQ [] 

-- ▪
isEmptyQ (MkQ xs) = null xs

-- ▪ 
queue x (MkQ xs) = 
	if null xs 
		then x:xs 
		else head xs : queue (tail xs)

-- Alternativa ➡ queue x (MkQ xs) = MkQ (xs ++ [x]) 

-- ▪
firstQ (MkQ (x:xs)) = x 

-- ▪
dequeue (MkQ (x:xs)) = MkQ xs 