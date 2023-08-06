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
firstQ :: Queue a -> a 

-- Describe la queue dada sin su primer elemento.
dequeue :: Queue a -> Queue a 

-- 1.
--
largoQ :: Queue a -> Int 
largoQ q = 
	if isEmptyQ q 
		then 0 
		else 1 + largoQ (dequeue q)

-- 2.
-- 
atender :: Queue Persona -> [Persona]
atender ps = 
	if isEmptyQ ps 
		then []
		else firstQ ps : atender (dequeue ps) 

-- 3.
unirQ :: Queue a -> Queue a -> Queue a
unirQ q1 emptyQ = q1  
unirQ q1 q2     = unirQ (queue (firstQ q2) q1) (dequeue q2)