-- Breve repaso de 🌳🌳🌳

data Arbol = Nil 
		   | Bin Arbol Arbol
		   deriving Show

arbolA :: Tree Int
arbolA = NodeT 1 (NodeT 2 (Empty) (NodeT 3 (NodeT 4 (Empty) (Empty)) (Empty))) (NodeT 1 (Empty) (Empty))

-- Pequeñas funciones sobre árboles 🥐
-- 1.
-- Indica si el árbol dado es Nil (vacío).
esNil :: Arbol -> Bool
esNil Nil 			= True 
esNil (Bin izq der) = False 

-- 2.
-- Describe el hijo izquierdo del árbol dado.
-- Pre: El árbol dado no debe ser Nil (vacío).
hijoIzq :: Arbol -> Arbol
hijoIzq (Bin izq der) = izq

-- 3.
-- Describe el hijo derecho del árbol dado.
-- Pre: El árbol dado no debe ser Nil (vacío).
hijoDer :: Arbol -> Arbol
hijoDer (Bin izq der) = der 

-- 4.
-- Describe la cantidad de nodos del árbol dado.
cantNodos :: Arbol -> Int 
cantNodos Nil           = 0 
cantNodos (Bin izq der) = 
	1 + cantNodos izq + cantNodos der 

-- 5.
-- Describe la cantidad de hojas del árbol dado.
cantHojas :: Arbol -> Int 
cantHojas Nil           = 0 
cantHojas (Bin izq der) =
	if esNil izq && esNil der 
		then 1 
		else cantHojas izq + cantNodos der

-- 6.
-- Describe la altura del árbol (rama más larga).
altura :: Arbol -> Int 
altura Nil = 0 
altura (Bin izq der) = 
	1 + max (altura izq) (altura der) 

-- 
-- TP 3.
data Tree a = Empty  
		    | NodeT a (Tree a) (Tree a)
			  deriving Show

-- 1. 
-- Describe la suma de los elementos de un árbol binario de enteros.
sumarT :: Tree Int -> Int 
sumarT Empty             = 0
sumarT (NodeT n izq der) = 
	n + sumarT izq + sumarT der    

-- 2.
-- Describe la cantidad de elementos (el tamaño) del árbol binario dado. 
sizeT :: Tree a -> Int 
sizeT Empty  			= 0 
sizeT (NodeT n izq der) = 
	1 + sizeT izq + sizeT der 

-- 3.
-- Dado un árbol de enteros, describe un nuevo árbol con el doble de cada número del árbol dado.
mapDobleT :: Tree Int -> Tree Int 
mapDobleT Empty 			= Empty
mapDobleT (NodeT n izq der) = 
	NodeT (doble n) (mapDobleT izq) (mapDobleT der)

-- 4. 
-- Dado un árbol de direcciones, describe un nuevo árbol con la dirección opuesta para cada elemento del
-- árbol dado. 
mapOpuestoT :: Tree Dir -> Tree Dir 
mapOpuestoT Empty 				= Empty
mapOpuestoT (NodeT dir izq der) = 
	NodeT (opuesto dir) (mapOpuestoT izq) (mapOpuestoT der)

-- 5.
-- Dado un árbol de palabras, describe un nuevo árbol con la longitud de cada palabra.
mapLongitudesT :: Tree String -> Tree Int 
mapLongitudesT Empty 				   = Empty 
mapLongitudesT (NodeT palabra izq der) = 
	NodeT (len palabra) (mapLongitudesT izq) (mapLongitudesT der)

-- 6.
-- Indica si el elemento dado pertenece al árbol dado.
perteneceT :: Eq a => a -> Tree a -> Bool 
perteneceT _ Empty 			   = False 
perteneceT x (NodeT a izq der) = 
	x == a || perteneceT x izq || perteneceT x der 

-- 7.
-- 
aparicionesT :: Eq a => a -> Tree a -> Int 
aparicionesT _ Empty = 0
aparicionesT x (NodeT a izq der) = 
	if x == a
		then 1 + aparicionesT a izq + aparicionesT a der 
		else     aparicionesT a izq + aparicionesT a der 

-- 8.
-- Dado un árbol de personas describe el promedio de las edades de las mismas.
-- Pre: El árbol dado no debe ser vacío.
promedioEdadesT :: Tree Persona -> Int 
promedioEdadesT arbolDePersonas = 
	div (sumaEdadesT arbolDePersonas) (sizeT arbolDePersonas) 

-- Describe la suma de las edades de las personas del árbol dado.
sumaEdadesT :: Tree Persona -> Int 
sumaEdadesT Empty                   = 0
sumaEdadesT (NodeT persona izq der) = 
	edadP persona + (sumaEdadesT izq) + (sumaEdadesT der) 

-- 9.
-- Dados dos árboles describe un árbol t en el que ambos árboles son hijos de t, y en la raíz de
-- t se guarda la suma de todos los elementos de los hijos de t.
engancharYSumarEnRaiz :: Tree Int -> Tree Int -> Tree Int 
engancharYSumarEnRaiz arbolA arbolB =
	NodeT (sumarT arbolA + sumarT arbolB) arbolA arbolB 

-- 10.
-- Describe la cantidad de hojas que tiene el árbol dado.
-- Obs: Una hoja es un nodo que no tiene hijos.
leaves :: Tree a -> Int 
leaves Empty 			 = 0 
leaves (NodeT _ izq der) =
	if isEmpty izq && isEmpty der 
		then 1 
		else leaves izq + leaves der  

-- Indica si el árbol dado es Empty (vacío)
isEmpty :: Tree a -> Bool 
isEmpty Empty = True
isEmpty _     = False 

-- 11.
-- Describe la altura del árbol dado.
heightT :: Tree a -> Int 
heightT Empty 			  = 0 
heightT (NodeT _ izq der) = 
	1 + maximo (heightT izq) (heightT der) 

-- 12.
-- Dado un árbol describe el número de nodos del mismo que no son hojas.
nodesT :: Tree a -> Int 
nodesT Empty             = 0 
nodesT (NodeT a izq der) = 
	if isEmpty izq && isEmpty der 
		then 0 
		else 1 + nodesT izq + nodesT der 

--
nodesT' :: Tree a -> Int 
nodesT' tree = sizeT tree - leaves tree 

-- 13.
-- Dado un árbol describe el árbol resultante de intercambiar el hijo izquierdo con el hijo derecho
-- en cado nodo del árbol dado.
espejoT :: Tree a -> Tree a 
espejoT Empty 			  = Empty 
espejoT (NodeT a izq der) = 
	NodeT a (espejoT der) (espejoT izq)

-- 14.
-- Describe una lista que representa el resultado de recorrer el árbol dado en modo in-orden.
listInOrder :: Tree a -> [a]
listInOrder Empty 			  = []
listInOrder (NodeT a izq der) = 
	listInOrder izq ++ [a] ++ listInOrder der  

-- 15.
-- Describe una lista que representa el resultado de recorrer el árbol dado en modo pre-orden.
listPreOrder :: Tree a -> [a]
listPreOrder Empty 			   = [] 
listPreOrder (NodeT a izq der) = 
	a : (listPreOrder izq ++ listPreOrder der)

-- 16.
-- Describe una lista que representa el resultado de recorrer el árbol dado en modo post-orden.
listPostOrder :: Tree a -> [a]
listPostOrder Empty 			= []
listPostOrder (NodeT a izq der) = 
	listPostOrder izq ++ listPostOrder der ++ [a]  

-- 17.
-- Dado un árbol de listas describe la concatenación de todas esas listas. 
-- El recorrido debe ser in-orden.
concatenarListasT :: Tree [a] -> [a]
concatenarListasT Empty 			    = []
concatenarListasT (NodeT lista izq der) = 
	 concatenarListasT izq ++ lista ++ concatenarListasT der 

-- 18.
-- Dados un número n y un árbol describe la lista con los nodos de nivel n.
-- Obs: El primer nivel de un árbol (su raı́z) es 0.
-- Pre: Debe ser 0 <= n <= altura del árbol - 1.  
levelN :: Int -> Tree a -> [a]
levelN _ Empty 			   = []
levelN n (NodeT a izq der) = 
	if n == 0 
		then [a]
		else levelN (n-1) izq ++ levelN (n-1) der 

-- 19.
-- Dado un árbol describe una lista de listas donde cada elemento representa un nivel del árbol dado.
listPerLevel :: Tree a -> [[a]]
listPerLevel arbol = recolectarElementosDelNivelDesde 0 arbol

-- Describe una lista de listas donde cada elemento representa un nivel del árbol dado, desde el nivel k.
recolectarElementosDelNivelDesde :: Int -> Tree a -> [[a]]
recolectarElementosDelNivelDesde k arbol = 
	if k == heightT arbol 
		then []
		else [levelN k arbol] ++ recolectarElementosDelNivelDesde (k+1) arbol 

-- 20.
-- 
widthT :: Tree a -> Int
widthT arbol = maximoList (longitudesDeNivelesT arbol) 

-- 
longitudesDeNivelesT :: Tree a -> [Int]
longitudesDeNivelesT arbol = 
	longitudes (listPerLevel arbol) 

-- Extra:
-- Dada una lista de árboles describe la lista con todos los elementos de todos los árboles.
--
concatenarT :: [Tree a] -> [a] 
concatenarT []		 = [] 
concatenarT (as:ass) = aplanar (listPerLevel as) ++ concatenarT ass  

-- ----------------------------------------------------------------------------------- -- 
-- Expresiones aritméticas ✍🏼👨🏼‍🎓

data Expresion = Constante Int 
			   | ConsExpUnaria OpUn Expresion
			   | ConsExpBinaria Expresion OpBin Expresion   
			   deriving Show 

data OpUn  = Neg | Inc | Dec deriving Show
data OpBin = Suma | Resta | Mult | Div deriving Show 

-- 1.
-- Describe el valor de la expresión dada.
-- Pre: Cuando la operación es división la expresión que representa el divisor no debe ser 0.
eval :: Expresion -> Int 
eval (Constante a)       						  = a 
eval (ConsExpUnaria Neg expresion) 				  = - eval expresion
eval (ConsExpUnaria Inc expresion) 				  = eval expresion + 1 
eval (ConsExpUnaria Dec expresion)                = eval expresion - 1
eval (ConsExpBinaria expresionA Suma expresionB)  = eval expresionA + eval expresionB 
eval (ConsExpBinaria expresionA Resta expresionB) = eval expresionA - eval expresionB
eval (ConsExpBinaria expresionA Mult expresionB)  = eval expresionA * eval expresionB 
eval (ConsExpBinaria expresionA Div expresionB)   = div (eval expresionA) (eval expresionB) 


-- 2. 
-- Describe la expresión simplificada según los criterios establecidos en el enunciado.
-- Pre: Cuando la operación es división la expresión que representa el divisor no debe ser 0.
simplificar :: Expresion -> Expresion
simplificar (ConsExpBinaria expresionA Suma (Constante 0))  = expresionA 
simplificar (ConsExpBinaria (Constante 0) Suma expresionA)  = expresionA 
simplificar (ConsExpBinaria expresionA Resta (Constante 0)) = expresionA 
simplificar (ConsExpBinaria (Constante 0) Resta expresionA) = ConsExpUnaria Neg expresionA

simplificar (ConsExpBinaria expresionA Suma (Constante 1))  = ConsExpUnaria Inc expresionA 
simplificar (ConsExpBinaria expresionA Resta (Constante 1)) = ConsExpUnaria Dec expresionA 
simplificar (ConsExpBinaria (Constante 1) Resta expresionA) = ConsExpUnaria Neg (ConsExpUnaria Dec expresionA)

simplificar (ConsExpBinaria expresionA Mult (Constante 1))  = expresionA
simplificar (ConsExpBinaria (Constante 1) Mult expresionA)  = expresionA
simplificar (ConsExpBinaria expresionA Mult (Constante 0))  = Constante 0 
simplificar (ConsExpBinaria (Constante 0) Mult expresionA)  = Constante 0 

simplificar (ConsExpBinaria expresionA Div (Constante 1))   = expresionA
simplificar (ConsExpBinaria (Constante 0) Div expresionA)   = Constante 0


-- 
-- Funciones y Tipos auxiliares 🐱‍🏍 
-- Dados un elemento e y un árbol binario describe la cantidad de elementos del árbol que son iguales
-- a e.
data Dir = Norte 
		 | Este
		 | Sur
		 | Oeste
		   deriving Show

-- Describe la dirección opuesta a la dada.
opuesto :: Dir -> Dir
opuesto Norte = Sur 
opuesto Este  = Oeste 
opuesto Sur   = Norte 
opuesto Oeste = Este  

--
type Nombre = String
type Edad   = Int

data Persona = MKPersona Nombre Edad deriving Show

-- Describe el nombre de la persona dada.
nombreP :: Persona -> Nombre 
nombreP (MKPersona nombre edad) = nombre 

-- Describe la edad de la persona dada.
edadP :: Persona -> Edad  
edadP (MKPersona nombre edad) = edad 

--
-- Describe la longitud de la lista dada.
len :: [a] -> Int 
len []     = 0
len (x:xs) = 1 + len xs  

-- Describe el doble del número dado.
doble :: Int -> Int 
doble x = 2*x

-- Describe el número más grande entre los dos números dados.
maximo :: Int -> Int -> Int 
maximo x y = if x >= y then x else y 

-- Describe el número más grande en la lista de enteros dada.
-- Pre: La lista de números dada no debe ser vacía.
maximoList :: [Int] -> Int
maximoList [x] 	  = x  
maximoList (x:xs) = max x (maximoList xs)

-- Describe una lista con las longitudes de los elementos de la lista dada.
longitudes :: Foldable t => [t a] -> [Int]
longitudes []     = [0]
longitudes (x:xs) = length x : longitudes xs 

-- Describe la lista formada por todos los elementos de las listas de la lista de listas dada.
aplanar :: [[a]] -> [a]
aplanar []       = [] 
aplanar (xs:xss) = xs ++ aplanar xss 