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

variaciones :: Set Int -> Int -> Set [Int]
variaciones cs 0 = [[]]
variaciones cs k = union (agregarATodos (head cs) (variaciones cs (k-1))) (variaciones (tail cs) k) 


-- Funciones auxiliares 🐱‍🏍

type Set a = [a]

vacio = Set a 
vacio = []

pertenece :: Eq a => a -> Set a -> Bool
pertenece e [] = False
pertenece e (c:cs) = e == c || pertenece s cs 

agregar :: Eq a => a -> Set a -> Set a 
agregar e cs | pertenece e cs =     cs 
			 | otherwise      = e : cs

union :: Eq a => Set a -> Set a -> Set a 
unionC [] xs    = xs 
union (c:cs) xs = union (agregar c xs) xs   