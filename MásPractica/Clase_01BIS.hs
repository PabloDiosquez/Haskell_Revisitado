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