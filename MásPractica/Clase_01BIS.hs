-- Parte II

-- Pares
origen :: (Integer, Integer)
origen = (0, 0)

-- (1, True)       :: (Integer, Bool)
-- ('a', True)     :: (Char, Bool)
-- (1, (1, False)) :: (Integer, (Integer, Bool))

-- fst 👉🏼 Describe la primer componente del par.
-- snd 👉🏼 Describe la segunda componente del par.

-- ¿Qué tipo tienen estas funciones? 👉🏼 POLIMORFISMO 👮🏼‍

-- fst :: (a, b) -> a
-- snd :: (a, b) -> b

-- Dos maneras de escribir funciones sobre pares 👉🏼 PATTERN MATCHING 😻

dadoVuelta :: (a,b) -> (b, a)
dadoVuelta par = (snd par, fst par) 

dadoVuelta' :: (a, b) -> (b, a)
dadoVuelta' (x, y) = (y, x) 

-- Ejemplo 👽

f :: (a, (b, c)) -> ((a, b), c)
f p = ((fst p, fst (snd p)), snd (snd p))

f' :: (a, (b, c)) -> ((a, b), c)
f' (a, (b, c)) = ((a, b), c)

-- Tipos Enumerativos 🌳
-- Un tipo enumerativo está dado por un número finito de valores, cada uno de los 
-- cuales tiene un único constructor.

data Simpson = Homero 
			 | Marge
			 | Bart
			 | Lisa
			 | Maggie
			 deriving Show 

-- Las funciones sobre tipos enumerativos se definen usando Pattern Matching.

type Edad = Int -- Renombre de tipos 

edad :: Simpson -> Edad
edad Homero = 36
edad Marge  = 34
edad Bart   = 10
edad Lisa   = 8
edad Maggie = 1


-- ⚠ madre es una función parcial => tiene asociada una precondición no trivial.
-- Propósito:
-- ▪ Describe a la madre del Simpson dado.
-- Precondición: 
-- ▪ El Simpson dado debe ser Bart, Lisa o Maggie.
--
madre :: Simpson -> Simpson
madre Bart   = Marge
madre Lisa   = Marge
madre Maggie = Marge

-- Ejercicio 💪🏼

type Coordenada = (Int, Int)

data Direccion = Norte 
			   | Este
			   | Sur 
			   | Oeste
			   deriving Show

-- Propósito:
-- ▪ Describe la coordenada que resultaría de moverse una unidad hacia la dirección indicada.
-- Precondición: 
-- ▪ No tiene (es una función total).
-- Observaciones:
-- ▪ Las coordenadas se expresan como un par (latitud, longitud).
--
desplazar :: Coordenada -> Direccion -> Coordenada
desplazar c Norte  = (fst c + 1, snd c)
desplazar c Este   = (fst c, snd c + 1)
desplazar c Sur    = (fst c - 1, snd c)
desplazar c Oeste  = (fst c, snd c - 1) 