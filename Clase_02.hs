module Clase_02
where

-- FUNCIONES CON VARIABLES DE TIPO 🛸

-- Propósito: Describe el elemento dado por parámetro.
-- Precondiciones: No tiene (es una función total).

identidad :: t -> t 
identidad x = x


-- Propósito: Dados dos elementos describe el primero de ellos.
-- Precondiciones: No tiene (es una función total)

primero :: tx -> ty -> tx 
primero x y = x


-- Propósito: Dados dos elementos describe el segundo de ellos.
-- Precondiciones: No tiene (es una función total)

segundo :: tx -> ty -> ty
segundo x y = y 


-- Propósito: Describe el número 5 dados elementos cualesquiera.
-- Precondiciones: No tiene (es una función total).

constante5 :: tx -> ty -> tz -> Int  
constante5 x y z = 5 


-- Propósito: Indica si los elementos dados tienen el mismo tipo.
-- Precondiciones: No tiene (es una función total).
-- Observaciones: Dada la signatura de la función, ésta siempre describirá True.

mismoTipo :: t -> t -> Bool
mismoTipo x y = True


-- CLASES DE TIPOS ➡ Conjunto de tipos de datos a los que se les puede aplicar un conjunto de funciones
-- ¿Qué tipo tienen las siguientes funciones? 🥕

triple :: Num a => a -> a -- Proporcionado por Haskell 🙊
triple x = 3 * x 

maximo :: Ord a => a -> a -> a
maximo x y 
		| x >= y    = x 
		| otherwise = y 


distintos :: Eq a => a -> a -> Bool
distintos x y = x /= y 