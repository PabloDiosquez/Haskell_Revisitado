-- Breve repaso de 🌳🌳🌳

data Arbol = Nil 
		   | Bin Arbol Arbol
		   deriving Show

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