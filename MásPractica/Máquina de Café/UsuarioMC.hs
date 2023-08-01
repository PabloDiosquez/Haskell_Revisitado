
import MaquinaCafe

-- Describe la cantidad de cortados que se podrían consumir usando esta máquina de café.
cuantosCortados :: MaquinaCafe -> Int 
cuantosCortados mc = 
	if disponibleMC mc CafeCortado 
		then 1 + cuantosCortados (pedirCafe mc CafeCortado)
		else 0 

-- Describe el total recaudado en todas las máquinas de café del local.
totalRecaudacion :: [MaquinaCafe] -> Int 
totalRecaudacion [] 	  = 0 
totalRecaudacion (mc:mcs) = recaudacionMC mc 
								+ totalRecaudacion mcs 