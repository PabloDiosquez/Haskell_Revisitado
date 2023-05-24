module Clase_03
where

-- EJERCITACIÓN CONJUNTA

-- 1.
-- Propósito: Describe la cantidad de dígitos del número dado.
-- Precondiciones: El número dado debe ser >= 0.

cantidadDigitos :: Int -> Int 
cantidadDigitos n | n <= 9    = 1
				  | otherwise = 1 + cantidadDigitos (resto n)
				    where resto n = div n 10 


-- 2.
-- Propósito: Indica si todos los dígitos del número dado son 0's o 1's.
-- Precondiciones: El número dado debe ser >= 0.

esBinario :: Int -> Bool
esBinario 0 = True
esBinario n = (ultimoDigito n <= 1) && esBinario (div n 10)
				where ultimoDigito n = mod n 10
