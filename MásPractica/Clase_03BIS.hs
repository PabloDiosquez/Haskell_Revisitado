-- Tipos Algebraicos 👷🏼‍

-- Un mapa de transportes interurbanos se representa con el siguiente tipo de datos algebraico.

type Cod    = Int 
type Ciudad = String 
data Medio  = Tren | Micro 
				deriving Show

data Mapa   = Vacio 
	        | AT Cod Medio Ciudad Ciudad Int Mapa -- AgregarTransporte 
	          deriving Show

-- Cada transporte tiene un código único, un medio, un origen, un destino y una tarifa.

-- Ejemplo 
mapa1 :: Mapa 
mapa1 =
	AT 11 Micro "A" "B" 110 
		(AT 7 Tren "A" "B" 80
			(AT 13 Micro "B" "C" 40  
				(AT 5 Tren "C" "D" 150 Vacio)))

-- El orden del origen y del destino es irrelevante. Por ejemplo, en el mapa1 de arriba, el transporte 11 
-- puede ir desde "A" hasta "B" o desde "B" hasta "A". 

-- Algunas funciones... 🥐

-- Propósito:
-- Indica si el código dado identifica a algún transporte existente en el mapa dado.
-- Precondiciones:
-- No tiene (es una función total).
--
tranporteExistente :: Mapa -> Cod -> Bool 
tranporteExistente Vacio cod                           = False 
tranporteExistente (AT codTransporte _ _ _ _ mapa) cod = 
					codTransporte == cod || tranporteExistente mapa cod 