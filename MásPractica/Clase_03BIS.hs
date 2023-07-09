-- Tipos Algebraicos 👷🏼‍

-- Un mapa de transportes interurbanos se representa con el siguiente tipo de datos algebraico.

type Cod = Int 

type Ciudad = String 

data Medio = Tren | Micro deriving Show 

data Mapa = Vacio 
	      | AgregarTransporte Cod Medio Ciudad Ciudad Int Mapa 
	        deriving Show

-- Cada transporte tiene un código único, un medio, un origen, un destino y una tarifa.