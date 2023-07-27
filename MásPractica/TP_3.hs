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