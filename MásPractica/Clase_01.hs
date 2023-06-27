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
-- El tipo de las funciones
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
-- ◽ Describe el número total de segundos que hay en "dias" días, "horas" horas, "minutos" minutos
--   y "segundos" segundos.

segundosEnTotal :: Integer -> Integer -> Integer -> Integer -> Integer
segundosEnTotal dias horas minutos segundos = 
	segundosEn_Dias dias + segundosEn_Horas horas + segundosEn_Minutos minutos + segundos

segundosEn_Dias :: Integer -> Integer
segundosEn_Dias dias = segundosEnUnDia * dias 

segundosEn_Horas :: Integer -> Integer
segundosEn_Horas horas = segundosEnUnaHora * horas 

segundosEn_Minutos :: Integer -> Integer
segundosEn_Minutos minutos = segundosEnUnMinuto * minutos

segundosEnUnaHora :: Integer
segundosEnUnaHora = minutosEnUnaHora * segundosEnUnMinuto

segundosEnUnDia :: Integer
segundosEnUnDia = horasEnUnDia * segundosEnUnaHora

segundosEnUnMinuto :: Integer
segundosEnUnMinuto = 60

minutosEnUnaHora :: Integer
minutosEnUnaHora   = 60

horasEnUnDia :: Integer
horasEnUnDia       = 24 



 













