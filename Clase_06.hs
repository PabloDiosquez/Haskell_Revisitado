module Clase_06
where

-- maximo :: Int -> Int -> Int 
-- maximo x y 
	--	   | x >= y    = x
	--	   | otherwise = y


-- maximo3 :: Int -> Int -> Int -> Int 
-- maximo3 x y z = maximo (maximo x y) z


-- Operaciones 🎃

-- head :: [a] -> a

-- tail :: [a] -> [a]

-- (:) :: a -> [a] -> [a]


-- 1. 
-- Propósito: Describe la suma de los elementos de una lista de números enteros.
-- Precondiciones: No tiene (es una función total).

sumatoria :: [Int] -> Int 
sumatoria []      = 0
sumatoria numeros = sumatoria (tail numeros) + head (numeros)

-- 2.
-- Propósito: Describe la longitud de la lista dada.
-- Precondiciones: No tiene (es una función total).

longitud :: [a] -> Int 
longitud []    = 0
longitud lista = longitud (tail lista) + 1 

-- 3.
-- Propósito: Indica si el elemento dado pertenece a la lista dada.
-- Precondiciones: El elemento dado debe ser del mismo tipo que los elementos de la lista dada.

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []           = False 
pertenece elemento lista = elemento == head(lista) || pertenece elemento (tail lista) 

-- 4.
-- Escribir una expresión que denote la lista estrictamente decreciente de enteros que comienza
-- con el número 1 y termina con el número -100.

lista :: [Int] 
lista = [-1,-2 .. (-100)]

-- 5.
-- Definir la función primerMultiplode_En_ :: Int -> [Int] -> Int que indica el primer
-- elemento de la lista que es múltiplo de 45345 que encuentre en la lista.

-- Propósito: Describe el primer múltiplo del número dado en la lista dada.
-- Precondiciones: La lista dada debe tener al menos un múltiplo del número dado.

primerMultiploDe_En_ :: Int -> [Int] -> Int 
primerMultiploDe_En_ numero lista 
							      | esMultiploDe_ (head lista) numero = numero 
								  | otherwise                         = primerMultiploDe_En_ numero (tail lista)
								  where esMultiploDe_ x y = mod x y == 0

-- PATTERN MATCHING EN LISTAS 💀

-- Propósito:
-- ◽ Describe la suma de los elementos de la lista dada.
-- Precondiciones:
-- ◽ No tiene (es una función total).

sumatoria' :: [Int] -> Int 
sumatoria' []     = 0
sumatoria' (x:xs) = x + sumatoria' xs 


-- Propósito:
-- ◽ Describe la longitud de la lista dada, es decir, describe la cantidad de elementos que tiene la lista. 
-- Precondiciones:
-- ◽ No tiene (es una función total).

longitud' :: [a] -> Int 
longitud' []     = 0
longitud' (x:xs) = 1 + longitud' xs 

-- Propósito:
-- ◽ Indica si el elemento dado pertenece a la lista de elementos dada. 
-- Precondiciones:
-- ◽ El elemento dado debe ser del mismo tipo que el de los elementos de la lista dada.

pertenece' :: Eq a => a -> [a] -> Bool 
pertenece' _ []     = False 
pertenece' e (x:xs) = e == x || pertenece' e xs 

-- EJERCICIOS 🐦

-- 1. 
-- Propósito: 
-- ▪ Describe la productoria de los números de la lista dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).

productoria :: [Int] -> Int 
productoria []     = 1
productoria (x:xs) = x * productoria xs

-- 2.
-- Propósito: 
-- ▪ Describe la lista que resulta de sumar el número dado a la lista de números dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).

sumarN :: Int -> [Int] -> [Int]
sumarN _ []     = []
sumarN n (x:xs) = (n + x) : sumarN n xs

-- 3.
-- Propósito: 
-- ▪ Describe la lista que resulta de sumar el primer elemento a cada elemento de la lista dada. 
-- Precondiciones:
-- ▪ La lista dada debe ser no vacía.

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero (x:xs) = sumarN x (x:xs)

-- 5.

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | esPar x   = x : pares xs
			 | otherwise = pares xs 
			 where esPar x = mod x 2 == 0


-- 6.

quitar :: Int -> [Int] -> [Int]
quitar _ [] = []
quitar e (x:xs) | e == x    = xs 
				| otherwise = x : quitar e xs 


-- 7.

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas _ [] = []
quitarTodas e (x:xs) | e == x    = quitarTodas e xs 
					 | otherwise = x : quitarTodas e xs


-- 8.

hayRepetidos :: [Int] -> Bool 
hayRepetidos [] = False 
hayRepetidos (x:xs) = pertenece x xs || hayRepetidos xs 

-- 9.

eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal [] = []
eliminarRepetidosAlFinal (x:xs) = x : eliminarRepetidosAlFinal (quitarTodas x xs) 

-- 11.

eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio [] = []
eliminarRepetidosAlInicio (x:xs) | pertenece x xs = eliminarRepetidosAlInicio xs
								 | otherwise      = x : eliminarRepetidosAlInicio xs  

maximo :: [Int] -> Int
maximo xs = maximoAux (head xs) xs

maximoAux :: Int -> [Int] -> Int
maximoAux e [] = e 
maximoAux e (x:xs) | e >= x    = maximoAux e xs 
				   | otherwise = maximoAux x xs


minimo :: [Int] -> Int 
minimo xs = minimoAux (head xs) xs

minimoAux :: Int -> [Int] -> Int 
minimoAux e [] = e 
minimoAux e (x:xs) | e <= x    = minimoAux e xs 
				   | otherwise = minimoAux x xs 

-- 12.

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar xs = minimo xs : ordenar (quitar (minimo xs) xs)

ordenar' :: [Int] -> [Int]
ordenar' [] = [] 
ordenar' xs = maximo xs : ordenar' (quitar (maximo xs) xs)  

-- 13.

reverso :: [Int] -> [Int]
reverso [] = []
reverso xs = ultimo xs : reverso (quitar (ultimo xs) xs)

-- Precondición: 
-- ▪ La lista dada no debe ser vacía.

ultimo :: [Int] -> Int 
ultimo (x:xs) | longitud (x:xs) == 1 = x 
		      | otherwise        = ultimo xs 


-- 14.

concatenar :: [Int] -> [Int] -> [Int]
concatenar [] ys = ys 
concatenar xs ys = concatenar (quitar (ultimo xs) xs) (ultimo xs : ys)

-- 15.

zipi :: [a] -> [b] -> [(a,b)]
zipi xs ys | esVacia xs || esVacia ys = []  
zipi (x:xs) (y:ys) = (x, y) : zipi xs ys 
		
esVacia :: [a] -> Bool 
esVacia xs = longitud xs == 0 