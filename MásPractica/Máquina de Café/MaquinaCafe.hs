
module MaquinaCafe where 

--- Interfaz de la máquina de ☕

data TipoDeCafe = CafeSolo 
				| CafeDulce 
				| CafeCortado  

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
					  Int -- recaudación        (💲)

nuevaMC = MC 0 0 0 0 0

disponibleMC (MC agua leche cafe azucar _) tipoDeCafe = 
	agua   >= aguaRequerida tipoDeCafe 
 && leche  >= lecheRequerida tipoDeCafe 
 && cafe   >= cafeRequerido tipoDeCafe
 && azucar >= azucarRequerido tipoDeCafe

pedirCafe (MC agua leche cafe azucar recaudacion) tipoDeCafe = 
	MC 
	(agua   - aguaRequerida tipoDeCafe)
	(leche  - lecheRequerida tipoDeCafe)
	(cafe   - cafeRequerido tipoDeCafe)
	(azucar - azucarRequerido tipoDeCafe)
	(recaudacion + precioCafe)

mantenerMC _ = MC capacidadAgua 
				  capacidadLeche 
				  capacidadCafe 
				  capacidadAzucar 
				  0

recaudacionMC (MC _ _ _ _ recaudacion) = recaudacion

-- Funciones auxiliares 🐱‍🏍 

precioCafe :: Int 
precioCafe = 100  

capacidadAgua :: Int 
capacidadAgua = 20000

capacidadLeche :: Int 
capacidadLeche = 2000 

capacidadCafe :: Int 
capacidadCafe = 1000

capacidadAzucar :: Int 
capacidadAzucar = 1000 

aguaRequerida :: TipoDeCafe -> Int 
aguaRequerida CafeSolo    = 200 
aguaRequerida CafeDulce   = 200
aguaRequerida CafeCortado = 150

lecheRequerida :: TipoDeCafe -> Int 
lecheRequerida CafeSolo    = 0
lecheRequerida CafeDulce   = 0
lecheRequerida CafeCortado = 50

cafeRequerido :: TipoDeCafe -> Int 
cafeRequerido CafeSolo    = 40
cafeRequerido CafeDulce   = 40
cafeRequerido CafeCortado = 30

azucarRequerido :: TipoDeCafe -> Int 
azucarRequerido CafeSolo    = 0
azucarRequerido CafeDulce   = 12
azucarRequerido CafeCortado = 12