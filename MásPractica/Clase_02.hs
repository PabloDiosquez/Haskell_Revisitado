-- Listas ⚜

-- Operador cons :
-- (:) :: a -> [a] -> [a]

-- Escribir la lista [1,2,3,4] usando solamente el operador (:)
lista :: [Int]
lista = 1 : (2 : (3 : []))

-- Funciones observadoras 👁 -- Tipos de null, head y tail 💥

-- Propósito:
-- Describe el primer elemento de la lista dada.
-- Precondiciones:
-- La lista dada no debe ser vacía.
--
-- head :: [a] -> a 

-- Propósito:
-- Describe la lista dada sin el primer elemento.
-- Precondiciones:
-- La lista dada no debe ser vacía.
--
-- tail :: [a] -> [a]

-- Propósito:
-- Indica si la lista dada está vacía.
-- Precondiciones:
-- No tiene (es una función total).
--
-- null :: [a] -> Bool 

-- ¿Qué tipo tiene la siguiente función?

f :: [a] -> a -> a  
f x y = 
	if null x || null (tail x)
		then y 
		else head (tail x) 

-- f [1,2,3] 4 = 2 

-- Esquema de recursión estructural ⛷
-- Sirve para definir "recorridos" sobre listas. Más precisamente, para definir funciones.

-- f [] = valor para la lista vacía
-- f (x:xs) = alguna combinación de x con (f x)

-- ◽ Ejercicio 
-- Definir la función longitud - recibe un String y devuelve su longitud.

-- Propósito:
-- * Describe la longitud (cantidad de caracteres) del string dado.
-- Precondiciones:
-- * No tiene (es una función total).
--

-- type String = [Char]

-- longitud :: String -> Int 
-- longitud "" 	= 0 
-- longitud (c:cs) = 1 + longitud cs 

-- Generalizar su tipo.

longitud :: [a] -> Int 
longitud [] 	= 0
longitud (x:xs) = 1 + longitud xs   

-- ◽ Ejercicio
-- Definir la función maximo que recibe una lista de números no negativos y 
-- desvuelve el más grande.

-- Propósito:
-- ▪ Recibe una lista de números y describe el más grande.
-- Precondiciones:
-- ▪ La lista dada no debe ser vacía.
-- ▪ La lista no contiene números enteros negativos.
--
maximo :: [Int] -> Int 
maximo []     = 0
maximo (x:xs) = maximoEntre x (maximo xs) 

-- Propósito:
-- ▪ Recibe dos números enteros y describe el más grande.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
maximoEntre :: Int -> Int -> Int 
maximoEntre x y = if x >= y 
						then x 
						else y 

-- ◽ Ejercicio
-- Definir la función escalar - recibe un factor y una lista de números y devuelve la lista 
-- que resulta de multiplicar todos los elementos por ese factor.

-- Propósito:
-- ▪ Recibe un número entero (factor) y una lista de números enteros y describe la lista que
--   resulta de multiplicar todos los elementos de la lista por ese número dado.
-- Precondiciones:
-- ▪ 
-- 
escalar :: Int -> [Int] -> [Int]
escalar factor []     = [] 
escalar factor (x:xs) = 
	(factor * x) : escalar factor xs 

-- ◽ Ejercicio 
-- Definir la función losBuenos - que recibe una lista de pares y devuelve una lista con el primer
-- elemento de cada par, siempre que el segundo elemento del par sea True.

-- Propósito:
-- *
-- Precondiciones:
-- *
--
losBuenos :: [(a, Bool)] -> [a]
losBuenos []     = [] 
losBuenos (cabeza:cola) = 
	if snd cabeza 
		then fst cabeza : losBuenos cola
		else losBuenos cola 

-- ◽ Ejercicio

-- Definir la función append - recibe dos listas y devuelve una lista con todos los elementos
-- de ambas yuxtapuestos, en orden. 
-- Escribir el tipo de append.

-- Propósito:
-- * Describe la lista con todos los elementos de las dos listas dadas en orden.
-- Precondiciones:
-- * No tiene (es una función total).
--
append :: [a] -> [a] -> [a]
append [] ys     = ys 
append (x:xs) ys = x : append xs ys  

-- ◽ Ejercicio

-- Definir la función sinRepetidos - recibe una lista y devuelve la lista que contiene
-- los mismos elementos pero no tiene repetidos.

-- Propósito:
-- * Describe la lista que contiene los mismos elementos que la lista dada pero sin repetidos.
-- Precondiciones:
-- * No tiene (es una función total).
--
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos []     = []
sinRepetidos (x:xs) = 
	if pertenece x (sinRepetidos xs) 
		then        sinRepetidos xs 
		else   x :  sinRepetidos xs 


-- Propósito:
-- * Indica si el elemento dado pertenece a la lista dada.
-- Precondiciones:
-- * No tiene (es una función total).
--
pertenece :: Eq a => a -> [a] -> Bool 
pertenece _ []     = False 
pertenece e (x:xs) = e == x || pertenece e xs 

-- ◽ Ejercicio 
-- Definir la función incluida - recibe dos listas y denota True si todos los elementos de la primera
-- están contenidos en la segunda.

-- Propósito:
-- * Indica si todos los elementos de la primer lista dada están contenidos en la segunda lista dada.
-- Precondiciones:
-- * No tiene (es una función total).
--
incluida :: Eq a => [a] -> [a] -> Bool
incluida [] ys     = True  
incluida (x:xs) ys = pertenece x ys && incluida xs ys

-- ◽ Ejercicio
-- Definir la función diferencia - recibe dos listas. Devuelve una lista que tiene todos los 
-- elementos de que están en la primer lista y no están en la segunda lista.

-- Propósito:
-- * Describe la lista formada por todos los elementos de la primer lista dada que no estén contenidos
--   en la segunda lista dada.
-- Precondiciones:
-- * No tiene (es una función total).
--
diferencia :: Eq a => [a] -> [a] -> [a]
diferencia [] ys     = [] 
diferencia (x:xs) ys = 
	if pertenece x ys 
		then     diferencia xs ys 
		else x : diferencia xs ys 		 

-- ◽ Ejercicio
-- Definir la función sufijos - recibe una lista y denota la lista de listas de todos sus sufijos.

-- Propósito:
-- * Describe la lista de listas de todos los sufijos de la lista dada.
-- Precondiciones:
-- * No tiene (es una función total).
--
sufijos :: [a] -> [[a]]
sufijos []     = [[]]
sufijos (x:xs) = (x:xs) : sufijos xs 


-- ◽ Ejercicio
-- Definir la función prefijos - recibe una lista y denota la lista de listas de todos sus prefijos.

-- Ejemplo
-- prefijos [2,3]   = [[2,3], [2], []]
-- prefijos [1,2,3] = [[1,2,3], [1,2], [1], []]

-- Propósito:
-- * Describe la lista de listas de todos los prefijos de la lista dada.
-- Precondiciones:
-- * No tiene (es una función total).
--
prefijos :: [a] -> [[a]]
prefijos []     = [[]]
prefijos (x:xs) = [] : agregarATodosAdelante x (prefijos xs) 

agregarATodosAdelante :: a -> [[a]] -> [[a]]
agregarATodosAdelante e []       = []
agregarATodosAdelante e (xs:xss) = (e:xs) : agregarATodosAdelante e xss 