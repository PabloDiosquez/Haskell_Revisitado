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