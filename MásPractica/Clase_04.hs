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

-----------------------------------------
-- Agencia de espías 🕵🏼‍
-- Las agencias de espías se estructuran en forma jerárquica. Hay dos 
-- tipos de espías:
-- 1. Los agentes son los espías que recolectan información de diversas
-- fuentes. Cada agente está radicado en una ciudad.
-- 2. Los jefes son los espías que coordinan la actividad de otros espías.
-- Cada jefe tiene dos espías a su cargo y pueden recibir órdenes de su propio
-- jefe (si lo tiene). Observar que un jefe puede tener a otro jefe a su cargo.

-- Todos los espías se identifican con un código numérico.
-- Modelaremos la agencia de espías con los siguientes tipos de datos algebraicos.

type Cod    = Int 

type Ciudad = String

data Agencia = Agente Cod Ciudad 
			 | Jefe Cod Agencia Agencia 
			   deriving Show

-- Por ejemplo:
agencia1 :: Agencia  
agencia1 = Jefe 1 
				(Jefe 2 
					(Jefe 3 
						(Agente 31 "Buenos Aires")
						(Agente 31 "Santiago de Chile"))
					(Jefe 4 
						(Agente 41 "Bruselas")
						(Agente 42 "Washington DC")))
				(Jefe 5 
					(Jefe 6 
						(Agente 61 "Bangkok")
						(Agente 62 "Amsterdam"))
					(Jefe 7 
						(Agente 71 "Bruselas")
						(Agente 72 "Bruselas")))

-- Estructura del 🌳 de 🕵🏼‍

--			       	   Jefe 1 
-- 				   /           \
--			      /             \
--               /               \
--         Jefe 2             	  Jefe 5
--          /  \                  /    \ 
--         /    \                /      \
--   Jefe 3      Jefe 4      Jefe 6      Jefe 7
--   /     \      /   \        / \         /  \
-- Ag 31 Ag 32  Ag 41 Ag 42  Ag 61 Ag 62  Ag 71 Ag 72 

-- Definir las siguientes funciones:

-- 1.
-- Propósito:
-- Describe el número de espías de la agencia dada.
nroEspias :: Agencia -> Int 
nroEspias (Agente cod ciudad) 		   = 1
nroEspias (Jefe cod agencia1 agencia2) = 
	1 + nroEspias agencia1 + nroEspias agencia2

-- 2.
-- Propósito:
-- Indica si el espía identificado por el código dado
-- pertenece a la agencia dada.
esEspiaDe :: Agencia -> Cod -> Bool
esEspiaDe (Agente cod ciudad) cod'          = cod == cod'
esEspiaDe (Jefe cod agencia1 agencia2) cod' = 
	cod == cod' 			||
	esEspiaDe agencia1 cod' ||
	esEspiaDe agencia2 cod'   

-- 3.
-- Propósito:
-- Describe una lista con los códigos que identifican a los
-- agentes de la agencia dada que están radicados en la ciudad 
-- dada.
agentesRadicadosEn :: Agencia -> Ciudad -> [Cod]
agentesRadicadosEn (Agente cod ciudad) ciudad'          =     
	if ciudad == ciudad' 
		then [cod]
		else []  
agentesRadicadosEn (Jefe cod agencia1 agencia2) ciudad' = 
	agentesRadicadosEn agencia1 ciudad' ++
	agentesRadicadosEn agencia2 ciudad' 

-- 4.
-- Propósito:
-- Dada una agencia, describe la agencia que resulta de 
-- cambiar el código de un espía del código viejo al código nuevo.
-- 
renombrarEspia :: Agencia -> Cod -> Cod -> Agencia
renombrarEspia (Agente cod ciudad) codViejo codNuevo          = 
	Agente (renombrarCodigo cod codViejo codNuevo) ciudad
renombrarEspia (Jefe cod agencia1 agencia2) codViejo codNuevo = 
	Jefe (renombrarCodigo cod codViejo codNuevo)
	 (renombrarEspia agencia1 codViejo codNuevo)
	 (renombrarEspia agencia2 codViejo codNuevo)

-- Propósito:
-- Actualiza el código de un espía, en caso de ser necesario.
--
renombrarCodigo :: Cod -> Cod -> Cod -> Cod 
renombrarCodigo cod codViejo codNuevo = 
	if cod == codViejo
		then codNuevo
		else cod 

-- 5.
-- Propósito:
-- Dada una agencia y un código de espía, describe la subagencia
-- comandada por dicho espía.
-- Precondiciones:
-- Debe existir un espía con el código dado en la agencia.
--
subagencia :: Agencia -> Cod -> Agencia 
subagencia (Agente cod ciudad) cod'          = Agente cod ciudad
subagencia (Jefe cod agencia1 agencia2) cod' = 
	if cod == cod' 
		then Jefe cod agencia1 agencia2
		else if esEspiaDe agencia1 cod'
				then subagencia agencia1 cod' 
				else subagencia agencia2 cod'