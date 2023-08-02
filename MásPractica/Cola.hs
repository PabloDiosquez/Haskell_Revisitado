module Cola where

--- Interfaz del módulo Cola 🐀

-- Crea una cola vacía.
vaciaC 	    :: Cola a 

-- Indica si la cola dada está vacía.
estaVaciaC  :: Cola a -> Bool 

-- Agrega el elemento dado en el final de la cola dada.
encolarC    :: a -> Cola a -> Cola a

-- Describe el elemento de mayor prioridad de la cola dada.
-- Pre: La cola dada no debe estar vacía.
proximoC    :: Cola a -> a  

-- Elimina el elemento de mayor prioridad de la cola dada.
-- Pre: La cola dada no debe estar vacía.
desencolarC :: Cola a -> Cola a  

--- Implementación del módulo Cola 🕹
--  Primer Variante: el próximo elemento se encuentra al principio de la lista.

data Cola a = MkC [a]

vaciaC 				 = MkC []  -- O(1)

estaVaciaC (MkC xs)  = null xs -- O(1)

encolarC x (MkC xs)  = MkC (xs ++ [x]) -- encolarC es lineal, o sea si la cola tiene n elementos, cuesta O(n).

proximoC (MkC xs)    = head xs -- O(1)

desencolarC (MkC xs) = MkC (tail xs) -- O(1)