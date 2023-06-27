-- Ejemplo. Roque quiere cocinar pizza para n invitados, cada uno de los cuales como m porciones.
-- Una pizza tiene 8 porciones. Cada pizza requiere 250gr de harina. ¿Cuánta harina debería comprar? 

porcionesPorPizza :: Integer
porcionesPorPizza = 8

gramosDeHarinaPorPizza :: Integer
gramosDeHarinaPorPizza = 250

porcionesTotales :: Integer -> Integer -> Integer
porcionesTotales invitados porcionesPorInvitado = invitados * porcionesPorInvitado

pizzasTotales :: Integer -> Integer -> Integer
pizzasTotales invitados porcionesPorInvitado = 
	div (porcionesTotales invitados porcionesPorInvitado) porcionesPorPizza

{- Propósito: 
		Describe la cantidad de gramos de harina en total que hay que comprar para 
 		para preparar pizzas para n invitados, suponiendo que cada invitado come m
      	porciones de pizza.
-}
gramosDeHarinaTotales :: Integer -> Integer -> Integer
gramosDeHarinaTotales invitados porcionesPorInvitado = 
	(pizzasTotales invitados porcionesPorInvitado) * gramosDeHarinaPorPizza

-- ################################################################################
-- El tipo de las funciones 🐸

-- ¿Cuál es el tipo de la función f?
-- f x = x + 1

f :: Integer -> Integer 
f x = x + 1 

-- ¿Cuál es el tipo de la función g?
-- g x y = f x > f y

g :: Integer -> Integer -> Bool
g x y = f x > f y 

-- ¿Cuál es el tipo de la función h?
-- h b x y = b && x > y 

h :: Bool -> Integer -> Integer -> Bool 
h b x y = b && x > y 

-- Ejercicio
-- Roque quiere determinar cuántos segundos hay en d días, h horas, m minutos y s segundos.
-- Definir una función segundosEnTotal d h m s = ...

-- Propósito:
-- ◽ Describe el número total de segundos que hay en "d" días, "h" horas, "m" minutos
--   y "s" segundos.
segundosEnTotal :: Integer -> Integer -> Integer -> Integer -> Integer
segundosEnTotal d h m s = 60 * minutosEnTotal d h m + s 

-- Propósito:
-- ◽ Describe el número total de horas que hay en "d" días y "h" horas.
horasEnTotal :: Integer -> Integer -> Integer
horasEnTotal d h = 24 * d + h 

-- Propósito:
-- ◽ Describe el número total de minutos que hay en "d" días, "h" horas y "m" minutos.
minutosEnTotal :: Integer -> Integer -> Integer -> Integer 
minutosEnTotal d h m = 60 * horasEnTotal d h + m 

-- Solución alternativa 🙊

-- segundosEnTotal :: Integer -> Integer -> Integer -> Integer -> Integer
-- segundosEnTotal dias horas minutos segundos = 
-- 	segundosEn_Dias dias + segundosEn_Horas horas + segundosEn_Minutos minutos + segundos

-- segundosEn_Dias :: Integer -> Integer
-- segundosEn_Dias dias = segundosEnUnDia * dias 

-- segundosEn_Horas :: Integer -> Integer
-- segundosEn_Horas horas = segundosEnUnaHora * horas 

-- segundosEn_Minutos :: Integer -> Integer
-- segundosEn_Minutos minutos = segundosEnUnMinuto * minutos

-- segundosEnUnaHora :: Integer
-- segundosEnUnaHora = 60 * segundosEnUnMinuto

-- segundosEnUnDia :: Integer
-- segundosEnUnDia = 24 * segundosEnUnaHora

-- segundosEnUnMinuto :: Integer
-- segundosEnUnMinuto = 60

-- 🟣 Alternativa condicional 

-- Definir la función 
-- minimo :: Integer -> Integer -> Integer 

-- Propósito:
-- ▪ Describe el número más chico entre los dos números dados.
-- Precondiciones:
-- ▪ No tiene (es una función total).
minimo :: Integer -> Integer -> Integer
minimo x y = if x < y 
				then x
				else y  

maximo :: Integer -> Integer -> Integer
maximo x y = if x > y 
			 	then x 
			 	else y 

-- Propósito:
-- ▪ Describe el número más chico entre los tres números dados.
-- Precondiciones:
-- ▪ No tiene (es una función total).
minimo3 :: Integer -> Integer -> Integer -> Integer
minimo3 x y z = minimo (minimo x y) z

-- Propósito:
-- ▪ Describe el número que quedaría en el medio de los tres números dados.
-- Precondiciones:
-- ▪ No tiene (es una función total).
medio3 :: Integer -> Integer -> Integer -> Integer
medio3 x y z =
	if  x < y 
		then (if x > z 
				then x 
				else minimo y z)
		else (if y > z 
				then y 
				else minimo x z)

-- Propósito:
-- ▪ Describe el número más grande entre los tres números dados.
-- Precondiciones:
-- ▪ No tiene (es una función total).
maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z = if x > (maximo y z) 
					then x
					else maximo y z

maximo3V2 :: Integer -> Integer -> Integer -> Integer
maximo3V2 x y z = maximo x (maximo y z)