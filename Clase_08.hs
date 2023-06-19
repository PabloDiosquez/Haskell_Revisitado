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

-- variaciones {4,7} 0 = [[]]
-- variaciones {4,7} 1 = [[4], [7]]
-- variaciones {4,7} 2 = [[4,4], [4,7], [7,4], [7,7]] 

variaciones :: Set Int -> Int -> Set [Int]
variaciones cs 0 = [[]]
variaciones cs k = agregarElementosAListas cs (variaciones cs (k-1))

agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementosAListas [] _       = [] 
agregarElementosAListas (c:cs) xss = union (agregarElementoAdelante c xss) (agregarElementosAListas cs xss) 

agregarElementoAdelante :: Int -> Set [Int] -> Set [Int]
agregarElementoAdelante _ []       = [] 
agregarElementoAdelante c (xs:xss) = agregar (c:xs) (agregarElementoAdelante c xss)

-- Ejercicio 3

-- insertarEn l 6 1 = 6 : l
-- insertarEn (x:xs) 6 2 = x : (6 : xs) --> insertarEn l 6 2 = x : (insertarEn xs 6 1)
-- insertarEn (x:xs) n i = x : (insertarEn xs n (i-1))

-- Propósito:
-- ◽Dados una lista l, un número n y una posición i (contando desde 1) describe una lista en donde se 
-- insertó n en la posición i de l y los elementos siguientes corridos en una posición.
-- Precondiciones:
-- ◽El índice i debe ser <= (longitud de la lista) + 1.

insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn xs n 1 = n : xs 
insertarEn xs n i = (head xs) : (insertarEn (tail xs) n (i-1)) 

insertarEnCadaPosicion :: [Int] -> Int -> Int -> Set [Int]
insertarEnCadaPosicion xs x 1 = agregar (insertarEn xs x 1) vacio 
insertarEnCadaPosicion xs x i = agregar (insertarEn xs x i) (insertarEnCadaPosicion xs x (i-1))

insertarEnCadaPosDeTodasLasListas :: Set [Int] -> Int -> Set [Int]
insertarEnCadaPosDeTodasLasListas [] c       = []
insertarEnCadaPosDeTodasLasListas (xs:xss) c = union (insertarEnCadaPosicion xs c (length xs + 1))
											   (insertarEnCadaPosDeTodasLasListas xss c)

permutaciones :: Set Int -> Set [Int]
permutaciones [] = [[]]
permutaciones (c:cs) = insertarEnCadaPosDeTodasLasListas (permutaciones cs) c

-- Funciones auxiliares 🐱‍🏍

type Set a = [a]

vacio :: Set a 
vacio = []

pertenece :: Eq a => a -> Set a -> Bool
pertenece e [] = False
pertenece e (c:cs) = e == c || pertenece e cs 

agregar :: Eq a => a -> Set a -> Set a 
agregar e cs | pertenece e cs =     cs 
			 | otherwise      = e : cs

union :: Eq a => Set a -> Set a -> Set a 
union [] xs    = xs 
union (c:cs) xs = union cs (agregar c xs)  

cardinal :: Set a -> Int 
cardinal vacio  = 0
cardinal (x:xs) = 1 +  cardinal xs 