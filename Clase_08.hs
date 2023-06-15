module Clase_08
where 
	
factorial :: Int -> Int 
factorial 0 = 1
factorial n = factorial(n-1)*n

-- Ejercicio 1

combinatorio :: Int -> Int -> Int 
combinatorio n k = div (factorial n) ((factorial k)*(factorial (n-k)))

combinatorio' :: Int -> Int -> Int
combinatorio' n 0 = 1  -- ¿Cuántos conjuntos de tamaño 0 existen? 
combinatorio' n k | n == k    = 1 
				  | otherwise = (combinatorio' (n-1) k) + (combinatorio' (n-1) (k-1))  

-- Ejercicio 2

-- Propósito: 
-- ◽ Describe todas las listas de longitud <<k>> a partir de elementos del
-- conjunto dado.
-- Precondiciones:
-- ◽ <<k>> debe ser >= 0.

-- variaciones {4,7} 0 = [[]]
-- variaciones {4,7} 1 = [[4], [7]]
-- variaciones {4,7} 2 = [[4,4], [4,7], [7,4], [7,7]] 

variaciones :: Set Int -> Int -> Set [Int]
variaciones cs 0 = [[]]
variaciones cs k = agregarElementosAListas cs (variaciones cs (k-1))

agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementosAListas [] _       = [] 
agregarElementosAListas (c:cs) xss = union (agregarElementoAdelante c xss) (agregarElementosAListas cs xss) 

agregarElementoAdelante :: Int -> Set [Int] -> Set [Int]
agregarElementoAdelante _ []       = [] 
agregarElementoAdelante c (xs:xss) = agregar (c:xs) (agregarElementoAdelante c xss)

-- Funciones auxiliares 🐱‍🏍

type Set a = [a]

vacio :: Set a 
vacio = []

pertenece :: Eq a => a -> Set a -> Bool
pertenece e [] = False
pertenece e (c:cs) = e == c || pertenece e cs 

agregar :: Eq a => a -> Set a -> Set a 
agregar e cs | pertenece e cs =     cs 
			 | otherwise      = e : cs

union :: Eq a => Set a -> Set a -> Set a 
union [] xs    = xs 
union (c:cs) xs = union cs (agregar c xs)   