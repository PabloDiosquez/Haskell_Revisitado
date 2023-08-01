
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

--- Implementación de la máquina de ☕

data MaquinaCafe = MC Int -- cantidad de agua   (cc)
					  Int -- cantidad de leche  (cc)
					  Int -- cantidad de café   (gr)
					  Int -- cantidad de azúcar (gr)
					  Int -- recaudación        ($)

nuevaMC = MC 0 0 0 0 0

disponibleMC (MC agua leche cafe azucar _) tipoDeCafe = 
	agua   >= aguaRequerida tipoDeCafe 
 && leche  >= lecheRequerida tipoDeCafe 
 && cafe   >= cafeRequerido tipoDeCafe
 && azucar >= azucarRequerido tipoDeCafe

aguaRequerida :: TipoDeCafe -> Int 
aguaRequerida CafeSolo    = ... 
aguaRequerida CafeDulce   = ...
aguaRequerida CafeCortado = ...

lecheRequerida :: TipoDeCafe -> Int 
lecheRequerida CafeSolo    = ...
lecheRequerida CafeDulce   = ...
lecheRequerida CafeCortado = ...

cafeRequerido :: TipoDeCafe -> Int 
cafeRequerido CafeSolo    = ... 
cafeRequerido CafeDulce   = ...
cafeRequerido CafeCortado = ...

azucarRequerido :: TipoDeCafe -> Int 
azucarRequerido CafeSolo    = ...
azucarRequerido CafeDulce   = ...
azucarRequerido CafeCortado = ...