-- 1. CONCEPTOS BÁSICOS.

-- 1. Defina las siguientes funciones

-- a) 
-- Propósito: Dado un número devuelve su sucesor.

sucesor :: Int -> Int 
sucesor n = n + 1

-- b)
-- Propósito: Dados dos números devuelve su suma utilizando la operación +.

suma :: Int -> Int -> Int 
suma n m = n + m

-- c)
-- Propósito: Dados dos números devuelve el mayor de estos.

mayor :: Int -> Int -> Int 
mayor n m = if n >= m then n else m

-- 2. Defina las siguientes funciones usando pattern matching.

-- a) 
-- Propósito: Describe True si el valor dado es False; describe False en caso contrario.

negar :: Bool -> Bool
negar True  = False
negar False = True  

-- b)
-- Propósito: Dados dos booleanos si ambos son True devuelve True, sino devuelve False.

andLogico :: Bool -> Bool -> Bool
andLogico True True = True
andLogico _ _       = False

-- c) 
-- Propósito: Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.

orLogico :: Bool -> Bool -> Bool
orLogico False False = False
orLogico _ _         = True

-- d)
-- Propósito: Dado un par de números devuelve la primera componente.

primera :: (Int, Int)-> Int 
primera (n, _) = n

-- e)
-- Propósito: Describe la segunda componente de un par de números dados.

segunda :: Int -> Int -> Int 
segunda _ m = m

-- f) 
-- Propósito: Dado un par de números describe su suma.

sumaPar :: (Int, Int) -> Int 
sumaPar (n, m) = suma n m 

-- g) 
-- Propósito: Dado un par de números devuelve el mayor de éstos.

maxDelPar :: (Int, Int) -> Int 
maxDelPar (n, m) = mayor n m 

--3. Defina las siguientes funciones polimórficas.

-- a)
-- Propósito: Dado algún elemento de algún tipo devuelve el mismo elemento.

loMismo :: a -> a
loMismo a = a

-- b)
-- Propósito: Dado un elemento de algún tipo devuelve el número 7.

siempreSiete :: a -> Int 
siempreSiete _ = 7

-- c)
-- Propósito: Dado un elemento de algún tipo devuelve un par con ese elemento en ambas componentes.

duplicar :: a -> (a, a)
duplicar a = (a, a)

-- d) 
-- Propósito: Dado un elemento de algún tipo devuelve una lista con este único elemento.

singleton :: a -> [a]
singleton a = [a]

-- 4. Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas.

-- a) 
-- Propósito: Describe True si la lista dada es vacía, sino describe False.

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

-- b) 
-- Propósito: Describe el primer elemento de una lista dada.
-- Precondición: La lista dada no está vacía.

head' :: [a] -> a 
head' (x:xs) = x 

-- c) 
-- Propósito: Dada una lista devuelve la lista sin el primer elemento.
-- Precondición: La lista dada no está vacía.

tail' :: [a] -> [a]
tail' (x:xs) = xs 

-- 2. RECURSIÓN

-- 2.1) Recursión sobre listas.

-- Defina las siguientes funciones utilizando recursión estructural sobre listas, salvo que se indique
-- lo contrario.

-- 1) 
-- Propósito: Describe la suma de los elementos de una lista de números dada.

sumatoria :: [Int] -> Int
sumatoria []     = 0 
sumatoria (x:xs) = x + sumatoria xs 

sumatoria' :: [Int] -> Int 
sumatoria' l = if isEmpty l then 0 else head' l + sumatoria' (tail' l)

-- 2) 
-- Propósito: Dada una lista de elementos de algún tipo devuelve el largo de esa lista, 
-- es decir, la cantidad de elementos que posee.

longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs 

-- 3) 
-- Propósito: Dada una lista de enteros, devuelve un número que es el promedio entre
-- todos los elementos de la lista.
-- Precondición: La lista no debe ser vacía.

promedio :: [Int] -> Int 
promedio l = div (sumatoria l) (longitud l)

-- 4) 
-- Propósito: Dada la lista de números enteros, devuelve la lista de sucesores de cada entero. 

mapSucesor :: [Int] -> [Int]
mapSucesor []     = []
mapSucesor (x:xs) = sucesor x : mapSucesor xs

-- 5) 
-- Propósito: Dada una lista de pares de enteros, devuelve una nueva lista en la que cada
-- elemento es la suma de los elementos de cada par.

mapSumaPar :: [(Int, Int)] -> [Int]
mapSumaPar []     = []
mapSumaPar (x:xs) = sumaPar x : mapSumaPar xs

-- 6) 
-- Propósito: Dada una lista de pares, devuelve una nueva lista en la que cada elemento es el mayor de
-- las componentes de cada par. 

mapMaxDelPar :: [(Int, Int)] -> [Int]
mapMaxDelPar []     = []
mapMaxDelPar (x:xs) = maxDelPar x : mapMaxDelPar xs

-- 7)
-- Propósito: Describe True si todos los elementos de la lista de booleanos dada son True.

todoVerdad :: [Bool] -> Bool
todoVerdad []     = True
todoVerdad (x:xs) = x && todoVerdad xs

-- 8) 
-- Propósito: Describe True si alguno de los elementos de la lista de booleanos dada es True.

algunaVerdad :: [Bool] -> Bool
algunaVerdad []     = False 
algunaVerdad (x:xs) = x || algunaVerdad xs

-- 9)
-- Propósito: Dado un elemento y una lista devuelve True si existe un elemento en la lista igual 
-- al elemento dado.
-- Precondición: Los elementos de la lista deben ser del mismo tipo que el elemento dado.

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []     = False
pertenece e (x:xs) = e == x || pertenece e xs 

-- 10)
-- Propósito: Dados un elemento e y una lista xs devuelve la cantidad de apariciones de e en xs.
-- Precondición: Los elementos de la lista deben ser del mismo tipo que el elemento dado.

apariciones :: Eq a => a -> [a] -> Int
apariciones _ []     = 0
apariciones e (x:xs) = if e == x then 1 + apariciones e xs
							     else apariciones e xs 

-- 11) 
-- Propósito: Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.

filtrarMenoresA :: Int -> [Int] -> [Int]
filtrarMenoresA _ [] = []
filtrarMenoresA n xs = if head' xs < n then head' xs : filtrarMenoresA n (tail' xs)
									   else filtrarMenoresA n (tail' xs)
-- 12)
-- Propósito: Dados un elemento y una lista filtra (elimina) todas las ocurrencias de ese elemento en la
-- lista.
-- Precondición: Los elementos de la lista deben ser del mismo tipo que el elemento dado.

filtrarElemento :: Eq a => a -> [a] -> [a]
filtrarElemento _ [] = []
filtrarElemento e xs = if e == (head' xs) then filtrarElemento e (tail' xs) 
										  else (head' xs) : filtrarElemento e (tail' xs)