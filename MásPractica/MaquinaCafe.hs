
module MaquinaCafe where 

--- Interfaz de la máquina de ☕

data TipoDeCafe = CafeSolo 
				| CafeDulce 
				| CafeCortado 
				 deriving Show  

-- Inicia una máquina de café nueva 
nuevaMC 	  :: MaquinaCafe

-- Indica si el tipo de café dado está disponible en la máquina de café dada. 
disponibleMC  :: MaquinaCafe -> TipoDeCafe -> Bool 

-- Pre: El tipo de café solicitado debe estar disponible en la máquina.
pedirCafe 	  :: MaquinaCafe -> TipoDeCafe -> MaquinaCafe  

mantenerMC    :: MaquinaCafe -> MaquinaCafe 

-- Describe cuánto recaudó la máquina desde la última vez que se hizo el mantenimiento.
recaudacionMC :: MaquinaCafe -> Int  

-- ------------------------------------------------------------
-- Desde el punto de vista del usuario 👨‍🦰

-- Describe la cantidad de cortados que se podrían consumir usando esta máquina de café.
cuantosCortados :: MaquinaCafe -> Int 

-- Describe el total recaudado en todas las máquinas de café del local.
totalRecaudacion :: [MaquinaCafe] -> Int 