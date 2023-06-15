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

type Set Int = [Int]  

-- Propósito: 
-- ◽ Describe todas las listas de longitud <<k>> a partir de elementos del
-- conjunto dado.
-- Precondiciones:
-- ◽ <<k>> debe ser >= 0.

variaciones :: Set Int -> Int -> Set [Int]
