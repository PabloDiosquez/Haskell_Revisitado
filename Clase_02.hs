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

pepe :: (Floating t, Eq t, Num u, Eq u) => t -> t -> u -> Bool 
pepe x y z = sqrt(x+y) == x && 3*z == 0

-- TUPLAS 🚌

-- Funciones de acceso a los valores de un par en Prelude

-- fst :: (a, b) -> a

-- snd :: (a, b) -> b 

-- Ejemplos

suma :: (Float, Float) -> (Float, Float) -> (Float, Float)
suma u v = ((fst u + fst v), (snd u + snd v))

-- Pattern matching sobre tuplas

suma' :: (Float, Float) -> (Float, Float) -> (Float, Float)
suma' (ux, uy) (vx, vy) = (ux + vx, uy + vy)

esOrigen :: (Float, Float) -> Bool
esOrigen (0, 0) = True
esOrigen (_, _) = False

angulo0 :: (Float, Float) -> Bool
angulo0 (_, 0) = True
angulo0 (_, _) = False

angulo45 :: (Float, Float) -> Bool 
angulo45 (x, y) = x == y

-- ¿Conviene tener dos parámetros escalares o un parámetro dupla?

normaVectorial1 :: Float -> Float -> Float
normaVectorial1 x y = sqrt(x**2 + y**2)

normaVectorial2 :: (Float, Float) -> Float 
normaVectorial2 (x, y) = sqrt(x**2 + y**2)

normaDeLaSuma1 :: (Float, Float) -> (Float, Float) -> Float
normaDeLaSuma1 u v = normaVectorial1 (fst s) (snd s)
					where s = suma u v

normaDeLaSuma2 :: (Float, Float) -> (Float, Float) -> Float
normaDeLaSuma2 u v = normaVectorial2 (suma' u v)

-- EJERCICIOS 👷🏼‍

-- 2)
-- Propósito: Describe el producto interno entre los dos vectores de R2 dados.
-- Precondiciones: No tiene (es una función total).

productoInterno :: (Float, Float) -> (Float, Float) -> Float 
productoInterno (ux, uy) (vx, vy) = ux * vx + uy * vy

-- 3) 
-- Propósito: Indica si cada coordenada del primer vector dado es menor a la
-- coordenada correspondiente del segundo vector dado-
-- Precondiciones: No tiene (es una función total).

todoMenor :: (Float, Float) -> (Float, Float) -> Bool 
todoMenor (ux, uy) (vx, vy) = ux < vx && uy < vy 

-- 7)
-- Propósito: Crea una tupla a partir de los dos elementos dados.
-- Precondiciones: No tiene (es una función total).
-- Observaciones: Debe funcionar para elementos de cualquier tipo.

crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)   