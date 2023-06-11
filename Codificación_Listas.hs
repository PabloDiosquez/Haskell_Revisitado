module Codificación_Listas
where 

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