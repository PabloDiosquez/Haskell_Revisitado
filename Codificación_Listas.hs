module Codificación_Listas
where 


-- EJERCICIOS 🥕

-- 1) 
-- Propósito:
-- * Describe la longitud de la lista codificada por el número natural dado.
-- Precondiciones:
-- * El número dado debe ser >= 0.
 
longitud :: Int -> Int 
longitud n = longitudDesde n 1


-- Propósito:
-- * Describe la longitud de la lista codificada por el número natural dado desde el k-ésimo primo.
-- Precondiciones:
-- * Los números dados deben ser > 0.

longitudDesde :: Int -> Int -> Int 
longitudDesde n k | k > n     = 0
				  | otherwise = longitudDesde (div n ((kEsimoPrimo)^(mayorPotenciaQueDivide n (kEsimoPrimo)))) (k+1) + 1
				  where kEsimoPrimo = nEsimoPrimo k


-- Propósito:
-- * Dados dos números naturales n y k, describe el exponente de la mayor potencia de k que divide a n.
-- Precondiciones:
-- * Los números dados deben ser > 0.

mayorPotenciaQueDivide :: Int -> Int -> Int 
mayorPotenciaQueDivide n k | esDivisible n k  = 1 + mayorPotenciaQueDivide (div n k) k 
						   | otherwise        = 0
						   where esDivisible n k = mod n k == 0   
 

-- 2)
-- Propósito:
-- * Dados dos números naturales n e i, describe el i-ésimo elemento de la lista que codifica n. 
--   Si el índice está fuera de rango, el programa describe 0.
-- Precondiciones:
-- * Los números dados deben ser > 0.

iesimo :: Int -> Int -> Int 
iesimo n i = mayorPotenciaQueDivide n (iEsimoPrimo)
			where iEsimoPrimo = nEsimoPrimo i 


-- 3)
-- Propósito:
-- * Describe el primer elemento de la lista codificada por el número dado.
-- Precondiciones:
-- * El número dado debe ser > 0.

headN :: Int -> Int 
headN n = mayorPotenciaQueDivide n 2 

-- FUNCIONES AUXILIARES 🎃

-- Propósito:
-- * Indica si el número dado tiene divisores desde <<d>>.
-- Precondiciones:
-- * <<d>> debe ser <= que el número dado.

tieneDivisoresDesde :: Int -> Int -> Bool
tieneDivisoresDesde n d | d == n    = False   
					    | otherwise = esDivisible n d || tieneDivisoresDesde n (d+1)
					    where esDivisible n d = mod n d == 0

-- Propósito:
-- * Indica si el número dado es un número primo.
-- Precondiciones:
-- * El número dado debe ser >= 0.

esPrimo :: Int -> Bool 
esPrimo n = (n > 1) && not (tieneDivisoresDesde n 2)


-- Propósito:
-- * Describe el menor número primo desde el número dado.
-- Precondiciones:
-- * No tiene (es una función total).

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n 
				   | otherwise = minimoPrimoDesde (n+1)


-- Propósito:
-- * Describe el primo en la posición <<n>>.
-- Precondiciones:
-- * <<n>> debe ser > 0.

nEsimoPrimo :: Int -> Int 
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (nEsimoPrimo (n-1) + 1)