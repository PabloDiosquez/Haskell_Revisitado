module Clase_05
where 

-- Recursión con funciones auxiliares 🐱‍🏍

-- 1.
-- Propósito: 
-- ▪ Describe la suma de los divisores de n hasta h.
-- Precondiciones: 
-- ▪ Los números dados deben ser >= 0.
--
sumaDivisoresHasta :: Int -> Int -> Int 
sumaDivisoresHasta _ 0 = 0 
sumaDivisoresHasta n h 
	| esDivisible n h = sumaDivisoresHasta n (h-1) + h 
	| otherwise       = sumaDivisoresHasta n (h-1)


-- Una pequeña variante ... 🎃

-- Propósito: 
-- ▪ Describe la suma de los divisores de n desde d.
-- Precondiciones: 
-- ▪ Los números dados deben ser >= 0.
--
sumaDivisoresDesde :: Int -> Int -> Int 
sumaDivisoresDesde n d 
	| d == n        = n  
	| esDivisible n d = d + sumaDivisoresDesde n (d+1)
	| otherwise       =     sumaDivisoresDesde n (d+1) 


-- 2.
-- Propósito:
-- ▪ Describe la suma de los divisores del número dado.
-- Precondiciones: 
-- ▪ El número dado debe ser >= 0.
--
sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n 

sumaDivisores' :: Int -> Int 
sumaDivisores' n = sumaDivisoresDesde n 1

-- 3.
-- Propósito: 
-- ▪ Describe el menor divisor (mayor que 1) de un número natural n.
-- Precondiciones:
-- ▪ El número n debe ser >= 2.
--
menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2 

-- Propósito: Describe el menor divisor de un número natural n desde el número d.
-- Precondiciones: 
-- ▪ El número n debe ser >= 2.
-- ▪ El número d debe ser <= n.
--
menorDivisorDesde :: Int -> Int -> Int 
menorDivisorDesde n d 
	| esDivisible n d = d 
	| otherwise 	  = menorDivisorDesde n (d+1)

-- 4.
-- Propósito: 
-- ▪ Indica si el número n es primo o no.
-- Precondiciones: 
-- ▪ El número n debe ser un número >= 0.
--
esPrimo :: Int -> Bool 
esPrimo n = (n > 1) && (menorDivisor n == n)

-- Una versión alternativa de la función esPrimo ... 🦜  
--
esPrimo' :: Int -> Bool
esPrimo' n = (n > 1) && not (tieneDivisoresPropios n)

-- 5.
-- Propósito:
-- ▪ Describe el primo que ocupa la n-ésima posición ( el primer primo es el 2, el segundo es el 3,
--   el tercero es el 5, etc.))
-- Precondiciones:
-- ▪ El número n debe ser >= 1.
--
nEsimoPrimo :: Int -> Int 
nEsimoPrimo 1 = 2 
nEsimoPrimo n = menorPrimoDesde (nEsimoPrimo (n-1) + 1)

-- Propósito:
-- ▪ Describe el menor primo desde el número d.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
menorPrimoDesde :: Int -> Int 
menorPrimoDesde d 
	| esPrimo d = d 
	| otherwise = menorPrimoDesde (d+1) 

-- 6.
-- Propósito:
-- Dado un número m, describe el mínimo n, n >= m tal que n = k!, para algún k natural.
-- Precondiciones:
-- No tiene (es una función total).
--  
menorFactorialDesde :: Int -> Int 
menorFactorialDesde m = menorFactorialDesdeDesde m 1 

menorFactorialDesdeDesde :: Int -> Int -> Int 
menorFactorialDesdeDesde m k = 
	if factorial k >= m 
		then factorial k 
		else menorFactorialDesdeDesde m (k+1)

-- 7.
-- Propósito:
-- Dado un número m, describe el máximo n, con n <= m tal que n = k!, para algún k natural.
-- Precondiciones:
-- El número dado debe ser >= 0.
--
mayorFactorialHasta :: Int -> Int 
mayorFactorialHasta m = mayorFactorialHastaDesde m 1 

mayorFactorialHastaDesde :: Int -> Int -> Int 
mayorFactorialHastaDesde m k = 
	if factorial k >= m 
		then factorial(k-1)
		else mayorFactorialHastaDesde m (k+1)

-- 8.
-- Propósito:
-- Indica si el número dado es el factorial de algún número natural. Es decir, dado n natural
-- indica si existe un k natural tal que n = k!.
-- Precondiciones:
-- El número dado debe ser >= 0.
--
esFactorial :: Int -> Bool 
esFactorial n = menorFactorialDesde n == n 

-- 9.
-- Propósito:
-- Indica si el número dado es un número de Fibonacci.
-- Precondiciones:
-- El número dado debe ser >= 0.
--
esFibonacci :: Int -> Bool 
esFibonacci n = menorFibonacciDesde n == n 

menorFibonacciDesde :: Int -> Int 
menorFibonacciDesde n = menorFibonacciDesdeDesde n 1 

menorFibonacciDesdeDesde :: Int -> Int -> Int 
menorFibonacciDesdeDesde n k 
	| fibo k >= n = fibo k 
	| otherwise   = menorFibonacciDesdeDesde n (k+1)

-- 10.
-- Propósito:
-- Indica si el número dado es igual a la suma de los m primeros números primos, para algún m.
-- Precondiciones:
-- El número dado debe ser >= 0.
--
esSumaInicialDePrimos :: Int -> Bool 
esSumaInicialDePrimos n = menorSumaInicialDePrimosDesde n == n 

--
menorSumaInicialDePrimosDesde :: Int -> Int 
menorSumaInicialDePrimosDesde n = menorSumaInicialDePrimosDesdeDesde n 1 

--
menorSumaInicialDePrimosDesdeDesde :: Int -> Int -> Int 
menorSumaInicialDePrimosDesdeDesde n k 
	| sumaInicialDePrimosHasta k >= n = sumaInicialDePrimosHasta k 
	| otherwise                       = menorSumaInicialDePrimosDesdeDesde n (k+1)


-- 13.
-- Propósito:
-- Indica si el número dado puede escribirse como suma de dos números primos.
-- Precondiciones:
-- El número dado debe ser > 0.
-- 
esSumaDeDosPrimos :: Int -> Bool
esSumaDeDosPrimos n = esSumaDeDosPrimosAux n 0

--
esSumaDeDosPrimosAux :: Int -> Int -> Bool
esSumaDeDosPrimosAux n k | k > n = False
esSumaDeDosPrimosAux n k 
	| esPrimo k && esPrimo(n-k) = True 
	| otherwise                 = esSumaDeDosPrimosAux n (k+1) 

-- 14.
-- Conjetura de Christian Goldbach, 1742: Todo número par mayor que 2 puede escribirse como suma dedos
-- números primos. Escribir una función que pruebe la conjetura hasta un cierto punto.
-- goldbach :: Int -> Bool (hasta al menos 4.10**18 debería ser cierto).
--
goldbach :: Int -> Bool 
goldbach 4 = True 
goldbach n = 
	if esPar n 
		then esSumaDeDosPrimos n && goldbach(n-2)
		else goldbach(n-1)
		where esPar n = mod n 2 == 0  

-- 15.
-- Propósito:
-- Describe la cantidad de pares de primos gemelos (a, b) que verifican que b <= que el número dado.
-- Precondiciones:
-- El número dado debe ser > 0.
-- Observaciones:
-- Los números naturales a y b forman un par de primos gemelos si b = a+2 y tanto a como b son primos.
--
primosGem :: Int -> Int 
primosGem n = primosGemDesde n 1 

primosGemDesde :: Int -> Int -> Int
primosGemDesde n d = 
	if d > n
		then 0
		else if esPrimo(d-2) && esPrimo d  
				then 1 + primosGemDesde n (d+1)
				else     primosGemDesde n (d+1)

-- 16.
-- Conjetura de los primos gemelos: Existen infinitos pares de primos gemelos. Implementar la función
-- proxPrimosGem :: Int -> (Int, Int) que dado n devuelve el primer par de primos gemelos (a,b) tal que a > n.
--
proxPrimosGem :: Int -> (Int, Int) 
proxPrimosGem n = 
	if esPrimo n && esPrimo(n+2) 
		then (n, n+2)
		else proxPrimosGem (n+1)   

-- FUNCIONES AUXILIARES 🆘

-- Propósito: 
-- ▪ Indica si el primer número es divisible por el segundo.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
esDivisible :: Int -> Int -> Bool 
esDivisible x y = mod x y == 0

-- Propósito:
-- ▪ Indica si el número n tiene divisores propios. 
-- Precondiciones: 
-- ▪ El número n debe ser un número natural > 1.
--
tieneDivisoresPropios :: Int -> Bool 
tieneDivisoresPropios n = tieneDivisoresPropiosDesde n 2

-- Propósito:
-- ▪ Indica si el número n tiene divisores propios desde d.
-- Precondiciones:
-- ▪ El número n debe ser un número natural > 1.
-- ▪ El número d debe ser un número natural <= n.
--
tieneDivisoresPropiosDesde :: Int -> Int -> Bool 
tieneDivisoresPropiosDesde n d 
	| d == n    = False
	| otherwise = esDivisible n d || tieneDivisoresPropiosDesde n (d+1)

-- Propósito:
-- ▪ Describe el factorial del número dado.
-- Precondiciones:
-- ▪ El número dado debe ser >= 0.
--
factorial :: Int -> Int 
factorial 0 = 1 
factorial n = factorial(n-1)*n 

-- Propósito:
-- ▪ Describe el nésimo término de la sucesión de Fibonacci.
-- Precondiciones:
-- ▪ El número dado debe ser >= 0.
--
fibo :: Int -> Int 
fibo n 
	| n <= 1    = 1
	| otherwise = fibo(n-1) + fibo(n-2)

--
sumaInicialDePrimosHasta :: Int -> Int 
sumaInicialDePrimosHasta 0 = 0
sumaInicialDePrimosHasta n = sumaInicialDePrimosHasta(n-1) + nEsimoPrimo n