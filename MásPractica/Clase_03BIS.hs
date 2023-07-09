-- Tipos Algebraicos 👷🏼‍

-- Un mapa de transportes interurbanos se representa con el siguiente tipo de datos algebraico.

type Cod    = Int 
type Ciudad = String 
data Medio  = Tren | Micro 
				deriving Show

data Mapa   = Vacio 
	        | AT Cod Medio Ciudad Ciudad Int Mapa -- AgregarTransporte 
	          deriving Show

-- Cada transporte tiene un código único, un medio, un origen, un destino y una tarifa.

-- Ejemplo 
mapa1 :: Mapa 
mapa1 =
	AT 11 Micro "A" "B" 110 
		(AT 7 Tren "A" "B" 80
			(AT 13 Micro "B" "C" 40  
				(AT 5 Tren "C" "D" 150 Vacio)))

-- El orden del origen y del destino es irrelevante. Por ejemplo, en el mapa1 de arriba, el transporte 11 
-- puede ir desde "A" hasta "B" o desde "B" hasta "A". 

-- Algunas funciones... 🥐

-- 1️⃣ 
-- Propósito:
-- Indica si el código dado identifica a algún transporte existente en el mapa dado.
-- Precondiciones:
-- No tiene (es una función total).
--
transporteExistente :: Mapa -> Cod -> Bool 
transporteExistente Vacio cod                           = False 
transporteExistente (AT codTransporte _ _ _ _ mapa) cod = 
					codTransporte == cod || transporteExistente mapa cod 


-- 2️⃣
-- Propósito:
-- Describe la lista de las dos ciudades terminales (origen y destino) del transporte identificado
-- por el código dado en el mapa.
-- Precondiciones:
-- El código debe identificar a un transporte existente en el mapa.
--
terminales :: Mapa -> Cod -> [Ciudad]
terminales (AT codTransporte medio origen destino tarifa mapa) cod =
	if codTransporte == cod
		then [origen, destino]
		else terminales mapa cod   

-- 3️⃣
-- Propósito:
-- Describe la tarifa del transporte dado.
-- Precondiciones:
-- El código identifica a un transporte existente en el mapa.
--
tarifa :: Mapa -> Cod -> Int 
tarifa (AT codTransporte medio origen destino tarifaTransporte mapa) cod = 
	if codTransporte == cod 
		then tarifaTransporte 
		else tarifa mapa cod 

-- 4️⃣
-- Propósito:
-- Describe la lista de trenes que llegan a la ciudad indicada.
-- Precondiciones:
-- No tiene (es una función total).
--
trenesQueLlegan :: Mapa -> Ciudad -> [Cod]
trenesQueLlegan Vacio _ 										 = []
trenesQueLlegan (AT cod medio origen destino tarifa mapa) ciudad =
	if esTren medio && (ciudad == origen || ciudad == destino)
 		then cod : trenesQueLlegan mapa ciudad 
		else       trenesQueLlegan mapa ciudad 

-- Propósito:
-- Indica si el medio de transporte dado es un tren.
-- Precondiciones:
-- No tiene (es una función total).
--
esTren :: Medio -> Bool 
esTren Tren  = True
esTren _     = False 

-- 5️⃣
-- Propósito:
-- Describe un mapa que contiene la información de todos los mapas de la lista dada.
-- Precondiciones:
-- En la lista de mapas no hay dos transportes con el mismo código.
--
combinarMapas :: [Mapa] -> Mapa 
combinarMapas []           = Vacio 
combinarMapas (mapa:mapas) = combinarInformacionM mapa (combinarMapas mapas)


-- Propósito:
-- Combina la información de los dos mapas dados.
-- Precondiciones:
-- Los mapas no deben tener transportes con el mismo código.
--
combinarInformacionM :: Mapa -> Mapa -> Mapa 
combinarInformacionM Vacio mapa = mapa 
combinarInformacionM (AT cod mt o d t m) mapa = 
	combinarInformacionM m (AT cod mt o d t mapa)