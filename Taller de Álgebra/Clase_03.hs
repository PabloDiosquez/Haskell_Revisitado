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

-- 1.
-- Propósito: Describe el nésimo número de la secuencia de Tribonacci 😁
-- Precondiciones: El número dado debe ser >= 0. 

tribonacci :: Int -> Int 
tribonacci n 
			| n <= 2    = n  
			| otherwise = tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)


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
diabolico 0 = True 				   -- Case base elegido para mejorar la implementación del caso recursivo.
diabolico n = ultimoDigito n == 6 && diabolico (div n 10)
	
-- Propósito: Describe el último dígito del número dado.
-- Precondiciones: El número dado debe ser >= 0.

ultimoDigito :: Int -> Int 
ultimoDigito n = mod n 10 

-- 4.

-- Idea: digitosIguales n = digitosIguales (d1d2...dk) -> dk == d(k-1) && digitosIguales (d1d2...d(k-1))

-- Propósito: Indica si todos los dígitos del número dado son iguales.
-- Precondiciones: El número dado debe ser >= 0.

digitosIguales :: Int -> Bool
digitosIguales n =  (n <= 9) || (ultimoDigito n == (ultimoDigito (resto n))) && digitosIguales (resto n)

-- Propósito: Describe el número que resulta de eliminar el último dígito del número dado.
-- Precondiciones: El número dado debe ser >= 0.

resto :: Int -> Int 
resto n = div n 10

-- 6.
-- Propósito: Indica si un número n es potencia de otro m.
-- Precondiciones: Ambos números deben ser >= 0.

-- Idea:
-- 8 es potencia de 2 => 8 = 2*(👨‍👩‍👧‍👧) = 2*(2*(👬)) = ... = 2*(2*...(🕴🏼)))
-- n es potencia de m => n ´mod´ m == 0 && (n ´div´ m) es potencia de m 

es_PotenciaDe_ :: Int -> Int -> Bool 
es_PotenciaDe_ n m = (n == 1) || mod n m == 0 && es_PotenciaDe_ (div n m) m