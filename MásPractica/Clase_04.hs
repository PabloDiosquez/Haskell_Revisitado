-- Árboles binarios 🌳 ("sabor clásico")

data Arbol = Nil | Bin Arbol Arbol 
			 deriving Show 

-- Escribir todos los árboles binarios de 0, 1, 2 y 3 nodos.

-- 0️⃣  nodos ➡ Nil  
-- 1️⃣  nodo  ➡ Bin Nil Nil 
-- 2️⃣  nodos ➡ Bin (Bin Nil Nil) Nil || Bin Nil (Bin Nil Nil)
-- 3️⃣  nodos ➡ 
-- Bin (Bin (Bin Nil Nil) Nil) Nil   || 
-- Bin (Bin Nil (Bin Nil Nil)) Nil   || 
-- Bin (Bin Nil Nil) (Bin Nil Nil)   || 
-- Bin Nil (Bin (Bin Nil Nil) Nil)   ||
-- Bin Nil (Bin Nil (Bin Nil Nil))

-- Funciones sobre árboles ☂
-- Esquema de recursión estructural para el tipo de datos árbol.
-- f :: Arbol -> ...
-- f Nil 			= ...
-- f (Bin izq der) = ... f izq ... f der ...

-- Definir las siguientes funciones:
-- 1.
-- Propósito:
-- Indica si el árbol dado es Nil (vacío).
-- Precondiciones:
-- No tiene (es una función total).
--
esNil :: Arbol -> Bool 
esNil Nil = True 
esNil _   = False 

-- 2.
-- Propósito:
-- Describe el hijo izquierdo del árbol dado.
-- Precondiciones:
-- El árbol no debe ser Nil.
--
hijoIzq :: Arbol -> Arbol
hijoIzq (Bin izq der) = izq  

-- 3.
-- Propósito:
-- Describe el hijo derecho del árbol dado.
-- Precondiciones:
-- El árbol no debe ser Nil.
--
hijoDer :: Arbol -> Arbol  
hijoDer (Bin izq der) = der 

-- 4.
-- Propósito:
-- Describe la cantidad de nodos del árbol dado.
-- Precondiciones:
-- No tiene (es una función total)
--
cantidadDeNodos :: Arbol -> Int 
cantidadDeNodos Nil           = 0
cantidadDeNodos (Bin izq der) = 
	1 + cantidadDeNodos izq + cantidadDeNodos der

-- 5.
-- Propósito:
-- Describe la cantidad de hojas del árbol dado.
-- Precondiciones:
-- No tiene (es una función total).
-- 
cantidadDeHojas :: Arbol -> Int 
cantidadDeHojas Nil           = 0 
cantidadDeHojas (Bin izq der) = 
	if esHoja (Bin izq der) 
		then 1
		else cantidadDeHojas izq + cantidadDeHojas der 

-- Propósito:
-- Indica si el árbol dado es una hoja.
-- Precondiciones:
-- No tiene (es una función total).
--
esHoja :: Arbol -> Bool 
esHoja Nil = False 
esHoja (Bin izq der) = esNil izq && esNil der 

-- 6.
-- Propósito:
-- Describe la altura del árbol dado (rama más larga).
-- Precondiciones:
-- No tiene (es una función total). 
-- 
altura :: Arbol -> Int 
altura Nil           = 0 
altura (Bin izq der) = 1 + maximo (altura izq) (altura der)


-- Función auxiliar 🐱‍🏍

-- Propósito:
-- Describe el número más grande entre los dos dados.
-- Precondiciones:
-- No tiene (es una función total). 
-- 
maximo :: Int -> Int -> Int 
maximo x y = 
	if x >= y
	 then x
	 else y 