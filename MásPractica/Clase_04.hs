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
-- 
-- Precondiciones:
--
--
esNil :: Arbol -> Bool 
esNil Nil = True 
esNil _   = False 

-- 2.
-- Propósito:
-- 
-- Precondiciones:
--
--
hijoIzq :: Arbol -> Arbol
hijoIzq (Bin izq der) = izq  

-- 3.
-- Propósito:
-- 
-- Precondiciones:
--
--
hijoDer :: Arbol -> Arbol  
hijoDer (Bin izq der) = der 

-- 4.
-- Propósito:
-- 
-- Precondiciones:
--
--
cantidadDeNodos :: Arbol -> Int 
cantidadDeNodos Nil           = 0
cantidadDeNodos (Bin izq der) = 
	1 + cantidadDeNodos izq + cantidadDeNodos der

-- 5.
-- Propósito:
-- 
-- Precondiciones:
--
-- 
cantidadDeHojas :: Arbol -> Int 
cantidadDeHojas Nil           = 1 
cantidadDeHojas (Bin izq der) = 
	cantidadDeHojas izq + cantidadDeHojas der 

-- 6.
-- Propósito:
-- 
-- Precondiciones:
--
-- 
altura :: Arbol -> Int 
altura Nil           = 0 
altura (Bin izq der) = ...