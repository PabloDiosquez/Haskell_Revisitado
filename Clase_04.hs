module Clase_04
where 

-- Propósito: Describe la suma de los números desde el 1 hasta el número dado.
-- Precondiciones: El número dado debe ser >= 0.

sumatoria :: Int -> Int 
sumatoria 0 = 0
sumatoria n = sumatoria (n-1) + n

-- EJERCICIOS: OTRAS SUMATORIAS 

-- 1.

f1 :: Int -> Int
f1 0 = 1 
f1 n = f1 (n-1) + 2^n

-- 2.

f2 :: Int -> Float -> Float
f2 0 _ = 0  
f2 n q = f2 (n-1) q + q^n

-- 3.

f3 :: Int -> Float -> Float 
f3 0 _ = 0
f3 n q = f3 (n-1) q + q^(2*n - 1) + q^(2*n)  

-- 4.

f4 :: Int -> Float -> Float 
f4 n q = f3 n q - f2 (n-1) q 

-- 5.
-- Propósito: Describe el valor aproximado del número e a partir de 
-- la siguiente sumatoria hasta el número dado.
-- Precondiciones: El número dado debe ser >= 0.

eAprox :: Int -> Float 
eAprox 0 = 1
eAprox n = eAprox (n-1) + 1 / fromIntegral(factorial n)


-- Se define la constante e :: Float como la aproximaci´on de e a partir de los primeros 10
-- términos de la serie anterior

e :: Float 
e = eAprox 10  

-- EJERCICIOS: SUMATORIAS DOBLES 

-- 1. 

f :: Int -> Int -> Int 
f 0 m = 0 
f n m = f (n-1) m + round(f2 m (fromIntegral n))

-- 2.
-- Propósito: Describe la suma de todas las potencias de la forma
-- q^a+b con 1 ≤ a ≤ n y 1 ≤ b ≤ m.
-- Precondiciones: Los números n y m dados deben ser >= 0.
 
sumaPotencias :: Float -> Int -> Int -> Float 
sumaPotencias q n 0 = 0  
sumaPotencias q n m = sumaPotencias q n (m-1) + (q^m)*(f2 n q)

-- 3.
-- Propósito: Implementar una función sumaRacionales n m que sume todos los números racionales de
-- la forma p/q con 1 ≤ p ≤ n y 1 ≤ q ≤ m.
-- Precondiciones: Los números n y m dados deben ser >= 0.

sumaRacionales :: Int -> Int -> Float 
sumaRacionales n 0 = 0
sumaRacionales n m = sumaRacionales n (m-1) + fromIntegral(sumatoria n) / (fromIntegral m) 

-- 4.

g1 :: Int -> Int -> Int
g1 i n 
	   | i > n     = 0 
	   | otherwise = g1 i (n-1) + i^n 

-- 5.
-- f(n,n) = g2(n) + h(n) => g2(n) = f(n,n) - h(n)

g2 :: Int -> Int 
g2 n = f n n - h n

h :: Int -> Int 
h 0 = 0
h n = h (n-1) + round(f2 (n-1) (fromIntegral n)) 

-- De otra manera ... 🎭

g2_aux :: Int -> Int -> Int 
g2_aux 0 _ = 0
g2_aux m n = g2_aux (m-1) n + m^n

g2' :: Int -> Int 
g2' 0 = 0
g2' n = g2 (n-1) + (g2_aux n n)

-- 6.

g3 :: Int -> Int 
g3 0 = 0 
g3 n 
	 | esPar n   = g3 (n-1) + 2^n
	 | otherwise = g3 (n-1)
	 where esPar n = mod n 2 == 0 

-- 7.

g4 :: Int -> Int 
g4 n | n <= 9           = 0
	 | digitosIguales n = g4 (n-1) + n 
	 | otherwise        = g4 (n-1) 


-- FUNCIONES AUXILIARES 🆘

-- Propósito: Describe el factorial del número dado.
-- Precondiciones: El número dado debe ser >= 0.

factorial :: Int -> Int 
factorial 0 = 1
factorial n = factorial (n-1) * n

-- Propósito: Indica si el número dado tiene todos sus dígitos iguales.
-- Precondiciones: El número dado debe ser >= 0.
-- Observaciones: Un número de sólo una cifra tiene todos sus dígitos iguales 😅.

digitosIguales :: Int -> Bool
digitosIguales n = (n <= 9) || (ultimoDigito n == ultimoDigito (resto n)) && digitosIguales (resto n)

-- Propósito: Describe el último dígito del número dado.
-- Precondiciones: El número dado debe ser >= 0.

ultimoDigito :: Int -> Int 
ultimoDigito n = mod n 10

-- Propósito: Describe el número que resulta de eliminar el último dígito del número dado. En caso de que el 
-- número sea de sólo un dígito, describe 0.
-- Precondiciones: El número dado debe ser >= 0.

resto :: Int -> Int 
resto n = div n 10 