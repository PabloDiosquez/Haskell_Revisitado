-- 1. 📚 CONCEPTOS BÁSICOS.

-- 1. 🔹 Defina las siguientes funciones

-- a) 
-- Propósito: Dado un número devuelve su sucesor.

sucesor :: Int -> Int 
sucesor n = n + 1

-- b)
-- Propósito: Dados dos números devuelve su suma utilizando la operación +.

suma :: Int -> Int -> Int 
suma n m = n + m

-- c)
-- Propósito: Dados dos números devuelve el mayor de éstos.

mayor :: Int -> Int -> Int 
mayor n m = if n >= m then n else m

-- Propósito: Dados dos números devuelve el menor de éstos.

menor :: Int -> Int -> Int 
menor n m = if n <= m then n else m

-- 2. 🔹 Defina las siguientes funciones usando pattern matching.

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

-- 2. 📚 RECURSIÓN

-- 2.1) 🔸 Recursión sobre listas.

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

-- 13)
-- Propósito: Dada una lista de listas, describe la lista de sus longitudes.

mapLongitudes :: [[a]] -> [Int]
mapLongitudes []       = []
mapLongitudes (xs:xss) = (longitud xs) : mapLongitudes xss

-- 14) 
-- Propósito: Dados un número natural n y una lista de listas, devuelve la lista de aquellas listas que tienen más
-- de n elementos.

longitudMayorA :: Int -> [[a]] -> [[a]]
longitudMayorA _ []       = []
longitudMayorA n (xs:xss) = if longitud xs > n then xs : longitudMayorA n xss else longitudMayorA n xss 

-- 16)
-- Propósito: Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
-- lista.

snoc :: [a] -> a -> [a]
snoc [] e      = [e]
snoc (x:xs) e  = x : snoc xs e 

-- 17) 
-- Propósito: Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
-- elementos de la segunda a continuación.
-- Precondición: Las listas deben contener elementos del mismo tipo.

append :: [a] -> [a] -> [a]
append cs []     = cs 
append cs (x:xs) = append (snoc cs x) xs 

-- 18)
-- Propósito: Dada una lista de listas, devuelve una única lista con todos sus elementos.

aplanar :: [[a]] -> [a]
aplanar []       = []
aplanar (xs:xss) = append xs (aplanar xss) 

-- 19)
-- Propósito: Dada una lista devuelve la lista con los mismos elementos de atrás para adelante.

reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = snoc (reversa xs) x 

-- 20) 
-- Propósito: Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
-- máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
-- las listas no necesariamente tienen la misma longitud.

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos xs ys = if xs == [] || ys == [] then [] 
				   else mayor (head' xs) (head' ys) : zipMaximos (tail' xs) (tail' ys) 

-- 21) 
-- Propósito: Dadas dos listas de enteros de igual longitud, devuelve una lista de pares (min; max), donde
-- min y max son el mínimo y el máximo respectivamente entre los elementos de ambas listas en la misma posición.
-- Precondición: Las listas deben tener la misma longitud.

zipSort :: [Int] -> [Int] -> [(Int, Int)]
zipSort [] _          = []
zipSort (x:xs) (y:ys) = maxMin x y : zipSort xs ys

-- Propósito: Dados dos números enteros, describe una tupla con el máximo y el mínimo de los mismos respectivamente.

maxMin :: Int -> Int -> (Int, Int)
maxMin x y = (mayor x y, menor x y)

-- 2.2 🔸 Recursión sobre números.
-- Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique
-- lo contrario.

-- 1) 
-- Propósito: Describe el factorial de un número dado.
-- Precondición: El número dado debe ser >= 0.

factorial :: Int -> Int 
factorial 0 = 1
factorial n = factorial(n-1) * n

-- 2) 
-- Propósito: Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
-- n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n 
				  | n < 1     = []
				  | otherwise = n : cuentaRegresiva (n-1) 

-- 3)
-- Propósito: Dado un número n devuelve una lista cuyos elementos sean los números entre 1 y n (incluidos).
-- Precondición: El número dado debe ser >= 0.

contarHasta :: Int -> [Int]
contarHasta 0 = []
contarHasta n = snoc (contarHasta (n-1)) n 

-- 4) 
-- Propósito: Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
-- Precondición: El número dado debe ser >= 0.

replicarN :: Int -> a -> [a]
replicarN 0 _ = []
replicarN n e = e : replicarN (n-1) e 

-- 5) 
-- Propósito: Dados dos números **desde** y **hasta** devuelve una lista cuyos elementos sean los números entre éstos (incluidos).
-- Precondición: Debe ser **desde** <= **hasta**.

desdeHasta :: Int -> Int -> [Int]
desdeHasta desde hasta = if hasta < desde then [] 
						 else snoc (desdeHasta desde (hasta - 1)) hasta

-- 6)
-- Propósito: Dados un número n y una lista xs, devuelve una lista con los primeros n elementos de xs.
-- Si xs posee menos de n elementos, se devuelve la lista completa.

takeN :: Int -> [a] -> [a]
takeN n xs = if n == 0 || isEmpty xs then [] else (head' xs) : takeN (n-1) (tail' xs) 

-- 7)
-- Propósito: Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
-- recibida. Si la lista posee menos de n elementos, se devuelve una lista vacía.

dropN :: Int -> [a] -> [a]
dropN n xs = if n == 0 || isEmpty xs then xs else dropN (n-1) (tail' xs)

-- 8) 
-- Propósito: Dados un número n y una lista xs, devuelve un par donde la primera componente es la lista
-- que resulta de aplicar takeN a xs, y la segunda componente el resultado de aplicar dropN a xs.

splitN :: Int -> [a] -> ([a], [a])
splitN n xs = (takeN n xs, dropN n xs) 

-- 🔹 Anexo con ejercicios adicionales
-- Ejercicios adicionales para seguir practicando. Defina las siguientes funciones:

-- 1) 
-- Propósito: Dada una lista xs de enteros devuelva una tupla de listas, donde la primera componente con-
-- tiene todos aquellos números positivos de xs y la segunda todos aquellos números negativos de xs.

particionPorSigno :: [Int] -> ([Int], [Int])
particionPorSigno xs = (listaDePositivos xs, listaDeNegativos xs)

-- Propósito: Dada una lista de números enteros devuelve la lista de aquellos que son positivos.

listaDePositivos :: [Int] -> [Int]
listaDePositivos []     = []
listaDePositivos (x:xs) = if x > 0 then x : listaDePositivos xs else listaDePositivos xs 

-- Propósito: Dada una lista de números enteros devuelve la lista de aquellos que son negativos.

listaDeNegativos :: [Int] -> [Int]
listaDeNegativos []     = []
listaDeNegativos (x:xs) = if x < 0 then x : listaDeNegativos xs else listaDeNegativos xs

-- 2) 
-- Propósito: Dada una lista de números enteros xs, devuelve una tupla de listas donde la primera componente
-- contiene a todos aquellos números pares de xs y la segunda a todos aquellos números impares de xs.

particionPorParidad :: [Int] -> ([Int], [Int])
particionPorParidad xs = (listaDePares xs, listaDeImpares xs)

-- Propósito: Describe una lista con todos los números pares de la lista de enteros dada.

listaDePares :: [Int] -> [Int]
listaDePares []     = []
listaDePares (x:xs) = if esPar x then x : listaDePares xs else listaDePares xs

-- Propósito: Describe una lista con todos los números impares de la lista de enteros dada.

listaDeImpares :: [Int] -> [Int]
listaDeImpares []     = []
listaDeImpares (x:xs) = if not (esPar x) then x : listaDeImpares xs else listaDeImpares xs

-- Propósito: Indica si el número dado es par.

esPar :: Int -> Bool
esPar x = mod x 2 == 0

-- 3) 
-- Propósito: Dada una lista devuelve cada sublista resultante de apliar tail en cada caso.

subtails :: [a] -> [[a]]
subtails [] = [[]] 
subtails xs = xs : subtails (tail xs) 

-- 5)
-- Propósito: Devuelve True si la primera lista es prefijo de la segunda.

esPrefijo :: Eq a => [a] -> [a] -> Bool
esPrefijo [] _  = True
esPrefijo xs cs = (head' xs) == (head' cs) && esPrefijo xs cs
 
-- 6)
-- Propósito: Devuelve True si la primera lista es sufijo de la segunda.

esSufijo :: Eq a => [a] -> [a] -> Bool
esSufijo [] _ = True 
esSufijo xs ys = last xs == last ys && esSufijo (init xs) (init ys) 