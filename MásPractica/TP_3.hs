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
	1 + max altura izq altura der 

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


-- Funciones y Tipos auxiliares 🐱‍🏍 
--
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

doble :: Int -> Int 
doble x = 2*x