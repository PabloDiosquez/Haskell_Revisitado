-- Variante de implementación del TAD Conjunto.

--INVARIANTE DE REPRESENTACION:
--- Dado un elemento 'e' perteneciente al conjunto, no existe otro 'x' perteneciente al conjunto que sea igual a 'e'.
---	Es decir, no hay elementos repetidos en el conjunto.

data Conjunto a = MkC [a] Int 

-- Describe el conjunto vacío.
vacioC :: Eq a => Conjunto a 
vacioC = MkC [] 0 

-- Describe un conjunto al cual el elemento dado pertenece.
agregarC :: Eq a => a -> Conjunto a -> Conjunto a 
agregarC e (MkC xs card) = 
	if perteneceC e xs 
		then MkC xs card 
		else MkC (x:xs) (card+1)

-- Indica si el elemento dado pertenece al conjunto dado.
perteneceC :: Eq a => a -> Conjunto a -> Bool
perteneceC _ vacioC         = False 
perteneceC e (MkC (x:xs) _) = 
	e == x || perteneceC e (MkC xs _) 

-- Describe la cantidad de elementos del conjunto dado.
cantidadC :: Eq a => Conjunto a -> Int 
cantidadC (MkC xs card) = card  

-- Describe un conjunto que resulta de borrar el elemento dado del conjunto dado.
-- Pre: El elemento dado pertence al conjunto dado.
borrarC :: Eq a => a -> Conjunto a -> Conjunto a
borrarC e (MkC (x:xs) card) = 
	if e == x 
		then MkC xs (card-1)
		else agregarC x (borrarC e (MkC xs (card-1)))

-- Describe la unión de los dos conjuntos dados.
unionC :: Eq a => Conjunto a -> Conjunto a -> Conjunto a 
unionC vacioC c 		   = c 
unionC (MkC (x:xs) card) c =
	unionC (MkC xs (card-1)) (agregarC x c) 

-- Describe una lista con los elementos del conjunto dado.
listaC :: Eq a => Conjunto a -> [a] 
listaC (MkC xs _) = xs 