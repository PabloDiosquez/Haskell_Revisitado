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