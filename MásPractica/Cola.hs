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

-- -------------------------------------
--- Implementación del módulo Cola 🕹

--  Primer Variante: el próximo elemento se encuentra al principio de la lista.

-- data Cola a = MkC [a]

-- vaciaC 				 = MkC []  		  -- O(1)

-- estaVaciaC (MkC xs)  = null xs         -- O(1)

-- encolarC x (MkC xs)  = MkC (xs ++ [x]) -- encolarC es lineal, o sea si la cola tiene n elementos, cuesta O(n).

-- proximoC (MkC xs)    = head xs 		  -- O(1)

-- desencolarC (MkC xs) = MkC (tail xs)   -- O(1)

-- -------------------------------------
--- Segunda Variante: el próximo elemento se encuentra al final de la lista.

-- data Cola a = MkC [a] 

-- vaciaC 				 = MkC [] 		-- O(1)

-- estaVaciaC (MkC xs)  = null xs 		-- O(1)

-- encolarC x (MkC xs)  = MkC (x:xs)    -- O(1)

-- proximoC (MkC xs)    = last xs 		-- O(n)

-- desencolarC (MkC xs) = init xs 		-- O(n)

-- 👁
-- last :: [a] -> a 
-- last (x:xs) =  
-- 		if null xs 
-- 			then x 
-- 			else last xs 

-- init :: [a] -> [a] 
-- init (x:xs) = 
-- 		if null xs 
-- 			then []
-- 			else x : init xs 

-- ---------------------------------------
--- Tercer Variante: con dos listas ➡ frente y dorso (front y back)

data Cola a = MkC [a] [a]

-- vaciaC, estaVaciaC son O(1)
-- encolarC es O(1)
-- proximoC y desencolarC son O(n) en el peor caso (amortizado ⚠)

vaciaC = MkC [] [] 

estaVaciaC (MkC	frente dorso) = null frente && frente dorso 

encolarC (MkC frente dorso) = MkC frente (x : dorso) -- O(1) 

proximoC (MkC frente dorso) = 
	if null frente 
		then last dorso   -- O(n)
		else head frente  -- O(1)

desencolarC (MkC frente dorso) = 
	if null frente
		then MkC tail (reverse dorso) [] -- O(n)
		else MkC (tail frente) dorso     -- O(1)

-- reverse se puede hacer en O(n)
-- reverse :: [a] -> [a] 
-- reverse xs = reverse' xs [] 

-- reverse' :: [a] -> [a] -> [a] 
-- reverse' [] ys     = ys 
-- reverse' (x:xs) ys = reverse' xs (x:ys) 