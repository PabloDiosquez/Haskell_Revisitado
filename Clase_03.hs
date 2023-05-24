module Clase_03
where

-- Propósito: Indica si el número dado es par.
-- Precondiciones: El número dado debe ser >= 0.

esPar :: Int -> Bool
esPar n = parAux (abs n)
		  where parAux n = n == 0 || not (parAux (n-1)) 

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


-- RECURIÓN INDIRECTA 🤷🏼‍

-- En rigor, una definición de f es recursiva si f aparece en alguna cadena de reducción...

esPar' :: Int -> Bool
esPar' 0 = True
esPar' n = esImpar' (n-1)

esImpar' :: Int -> Bool
esImpar' 0 = False
esImpar' n = esPar' (n-1)

-- esPar 3 -> esImpar 2 -> esPar 1 -> esImpar 0 -> False

-- EJERCICIOS 👷🏼‍

-- 2.
-- Propósito: Indica si el número dado es múltiplo de 3.
-- Precondiciones: El número dado debe ser >= 0.

esMultiploDe3 :: Int -> Bool
esMultiploDe3 n 
				| n == 0    = True
				| n <= 2    = False 
				| otherwise = esMultiploDe3 (n-3)

-- 3.

-- Idea: diabolico n = diabolico (d1d2...dk) -> dk == 6 && diabolico (d1d2...d(k-1))
 
-- Propósito: Indica si todos los dígitos del número dado son iguales a 6.
-- Precondiciones: El número dado debe ser > 0.

diabolico :: Int -> Bool
diabolico 0 = True 		-- Case base elegido para mejorar la implementación del caso recursivo.
diabolico n = ultimoDigito n == 6 && diabolico (div n 10)
			  where ultimoDigito n = mod n 10 
