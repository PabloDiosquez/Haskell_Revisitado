module Clase_01
where 

-- 1.
-- Propósito: Describe el doble del número dado.
-- Precondiciones: No tiene (es una función total).

doble :: Int -> Int 
doble x = 2 * x

-- 2.
-- Propósito: Describe la suma de los dos números dados.
-- Precondiciones: No tiene (es una función total).

suma :: Int -> Int -> Int
suma x y = x + y 

-- 3.
-- Propósito: Describe la norma de un vector de cooordenadas x1 y x2.
-- Precondiciones: No tiene (es una función total).

normaVectorial :: Float -> Float -> Float
normaVectorial x1 x2 = sqrt(x1**2 + x2**2)

-- 4.
-- Propósito: Describe el número 8.
-- Precondiciones: No tiene (es una función total). 

funcionConstante8 :: Float -> Int
funcionConstante8 x = 8

-- Propósito: Indica si el número dado es mayor a 9.
-- Precondiciones: No tiene (es una funión total).

esMayorA9 :: Int -> Bool 
esMayorA9 n = n > 9


-- 🎃 FUNCIONES DEFINIDAS POR CASOS

-- 5.
-- Propósito: Describe 1 si el número dado es cero; en caso contrario, describe 1.
-- Precondiciones: No tiene (es una función total).

esCero :: Int -> Int 
esCero n 
		 | n == 0    = 1
		 | otherwise = 0

-- 6.
-- Propósito: Describe 1 si el número dado es > a cero; en caso contrario, describe -1.
-- Precondiciones: El número dado debe ser distinto de 0.

signo :: Int -> Int 
signo n 
		| n > 0     = 1
		| otherwise = -1

-- 7.
-- Propósito: Describe 1 si el número dado es > a cero; describe 0 si el número dado es 0; de otra manera, describe -1.
-- Precondiciones: No tiene (es una función total).

signoExtendido :: Int -> Int
signoExtendido n 
				 | n > 0     = 1
				 | n == 0    = 0
				 | otherwise = -1

-- 8.
-- Propósito: Describe el máximo de los números dados.
-- Precondiciones: No tiene (es una función total).

maximo :: Int -> Int -> Int
maximo x y 
			| x >= y    = x
			| otherwise = y


maximoRac :: Float -> Float -> Float
maximoRac x y 
			| x >= y = x
			| otherwise = y


-- 9. ¿Qué hacen las siguientes funciones?

-- Propósito: Dado un número describe el número 5.
-- Precondiciones: El número dado debe ser >= 3.

f1 :: Int -> Int 
f1 n 
	 | n >= 3 = 5


-- Propósito: Describe el número 5 si el número dado es >= 3.
-- 			  Describe el número 8 si el número dado es <= 1.
-- Precondiciones: El número dado debe ser o bien >= 3 o bien <= 1.

f2 :: Int -> Int 
f2 n | n >= 3 = 5
	 | n <= 1 = 8


-- Propósito: Describe el número 5 si el número dado es >= 3.
-- 			  Describe el número 8 si el número dado es <= 1.
-- 			  Si el número dado es 2 la función no está definida.
-- Precondiciones: No tiene (es una función total).


f3 :: Int -> Int 
f3 n 
	| n >= 3 = 5
	| n == 2 = undefined
	| otherwise = 8


-- Propósito: 
-- Precondiciones: No tiene (es una función total).

f4 :: Int -> Int 
f4 n 
	| n >= 3 = 5
	| n <= 9 = 7


-- Propósito: 
-- Precondiciones: No tiene (es una función total). 

f5 :: Int -> Int 
f5 n
	| n <= 9 = 7
	| n >= 3 = 5

-- 🕹 PATTERN MATCHING

-- 10.
-- Propósito: Describe 1 si el número dado es 0; caso contrario, describe 0.
-- Precondiciones: No tiene (es una función total).

esCeroPM :: Int -> Int 
esCeroPM 0 = 1
esCeroPM _ = 0 


-- 11.
-- Propósito: Describe 1 si el número dado es > a cero; describe 0 si el número dado es 0;
-- de otra manera, describe -1.
-- Precondiciones: No tiene (es una función total).

signoPM :: Int -> Int 
signoPM 0 = 0
signoPM n 
		  | n > 0     = 1
		  | otherwise = -1

-- 12.
-- Propósito: dados dos números b y c, calcula la cantidad de soluciones reales de la ecuación cuadrática
-- X^2 + bX + c = 0.
-- Precondiciones: No tiene (es una función).

cantidadDeSoluciones :: Float -> Float -> Int
cantidadDeSoluciones b c 
						 | discriminante > 0  = 2
						 | discriminante == 0 = 1
						 | otherwise          = 0
						 where discriminante = b**2 - 4*c 

-- EJEMPLOS:

-- Recursión cruzada 🤞

-- Propósito: Indica si el número dado es par.
-- Precondiciones: El número dado debe ser >= 0.

esPar :: Int -> Bool
esPar 0 = True
esPar n = esImpar (n-1)


-- Propósito: Indica si el número dado es impar.
-- Precondiciones: El número dado debe ser >= 0.

esImpar :: Int -> Bool
esImpar 0 = False
esImpar n = esPar (n-1)

-- De otra manera ... 🎠

esPar2 :: Int -> Bool
esPar2 n = mod n 2 == 0

esImpar2 :: Int -> Bool
esImpar2 n = not (esPar2 n) 

-- Otro ejemplo más raro ... 🦇

funcionRara :: Float -> Float -> Bool -> Bool
funcionRara x y z = ( x >= y ) || z

-- Otras posibilidades, usando pattern matching:

-- funcionRara :: Float -> Float -> Bool -> Bool
-- funcionRara x y True = True
-- funcionRara x y False = x >= y

-- funcionRara :: Float -> Float -> Bool -> Bool
-- funcionRara _ _ True = True
-- funcionRara x y False = x >= y

-- EJERCICIOS 🚀

-- 1 absoluto: calcula el valor absoluto de un número entero.

absoluto :: Int -> Int 
absoluto x 
		   | x >= 0    = x 
		   | otherwise = -x

-- 2 maximoAbsoluto: devuelve el máximo entre el valor absoluto de dos números enteros.

maximoAbsoluto :: Int -> Int -> Int 
maximoAbsoluto x y = maximo (absoluto x) (absoluto y)

-- 3 maximo3: devuelve el máximo entre tres números enteros.

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z = maximo x (maximo y z)


-- 4 algunoEs0: dados dos números racionales, decide si alguno de los dos es igual a 0 (hacerlo
-- dos veces, una sin usar y otra usando pattern matching).

algunoEs0 :: Float -> Float -> Bool 
algunoEs0 x y = (x == 0) || (y == 0)

-- Pattern matching 👌🏼

algunoEs0' :: Float -> Float -> Bool 
algunoEs0' 0 _ = True
algunoEs0' _ 0 = True
algunoEs0' _ _ = False 

-- 5 ambosSon0: dados dos números racionales, decide si ambos son iguales a 0 (hacerlo dos
-- veces, una sin usar y otra usando pattern matching).

ambosSon0 :: Int -> Int -> Bool
ambosSon0 x y = (x == 0) && (y == 0)

-- Pattern matching 👌🏼

ambosSon0' :: Int -> Int -> Bool
ambosSon0' 0 0 = True
ambosSon0' _ _ = False 

-- 6 esMultiploDe: dados dos números naturales, decidir si el primero es múltiplo del segundo.

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod x y == 0

-- 7 digitoUnidades: dado un número natural, extrae su dígito de las unidades.

digitoUnidades :: Int -> Int 
digitoUnidades x = mod x 10

-- 8 digitoDecenas: dado un número natural, extrae su dígito de las decenas.

digitoDecenas :: Int -> Int 
digitoDecenas x = mod (div x 10) 10