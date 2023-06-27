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