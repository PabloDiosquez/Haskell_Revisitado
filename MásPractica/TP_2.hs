-- 1.
data Dir = Norte 
		 | Este
		 | Sur
		 | Oeste
	       deriving Show

-- Propósito:
-- ▪ Describe la dirección opuesta de la dirección dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
opuesto :: Dir -> Dir 
opuesto Norte = Sur 
opuesto Este  = Oeste
opuesto Sur   = Norte 
opuesto Oeste = Este 

-- Propósito:
-- ▪ Describe la dirección siguiente de la dirección dada, en sentido horario.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
siguiente :: Dir -> Dir 
siguiente Norte = Este 
siguiente Este  = Sur 
siguiente Sur   = Oeste
siguiente Oeste = Norte 

-- 2.
data Persona = MKPersona Nombre Edad 

type Nombre = String 
type Edad   = Int

-- Función constructora 👷🏼‍
-- Propósito:
-- ▪ Describe una persona con el nombre dado y edad = a 0.
-- Precondiciones:
-- ▪ El nombre dado no debe ser vacío.
--
nacer :: Nombre -> Persona
nacer nombre = MKPersona nombre 0 

-- Propósito:
-- ▪ Describe el nombre de la persona dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
nombre :: Persona -> Nombre
nombre (MKPersona nombre _)= nombre 

-- Propósito:
-- ▪ Describe la edad de la persona dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
edad :: Persona -> Edad
edad (MKPersona _ edad) = edad 

-- Propósito:
-- ▪ Describe a la persona dada con su edad aumentada en 1.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
crecer :: Persona -> Persona
crecer (MKPersona nombre edad) = MKPersona nombre (edad + 1)

-- Propósito:
-- ▪ Describe a la persona dada con un nuevo nombre.
-- Precondiciones:
-- ▪ "nombreNuevo" no debe ser vacío.
--
cambioDeNombre :: Nombre -> Persona -> Persona
cambioDeNombre nombreNuevo (MKPersona nombre edad) = MKPersona nombreNuevo edad 

-- Propósito:
-- ▪ Indica si la primer persona dada es menor que la segunda persona dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
esMenorQueLaOtra :: Persona -> Persona -> Bool 
esMenorQueLaOtra (MKPersona _ edad1) (MKPersona _ edad2) = edad1 < edad2

-- Propósito:
-- ▪ Describe la lista de las personas de la lista de personas dada cuyas edades superan
--   a la edad dada.
-- Precondiciones:
-- ▪ "edadASuperar" debe ser >= 0.
--
mayoresA :: Edad -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA edadASuperar (persona:personas) = 
	if (edad persona > edadASuperar)
	then persona : mayoresA edadASuperar personas
	else mayoresA edadASuperar personas

-- Propósito:
-- ▪ Describe el promedio de las edades de las personas de la lista dada.
-- Precondiciones:
-- ▪ La lista posee al menos una persona.
--
promedioEdad :: [Persona] -> Int 
promedioEdad personas = div (sumatoria (edades personas)) (longitud personas) 

edades :: [Persona] -> [Edad]
edades [] = []
edades (persona:personas) = edad persona : edades personas

-- Propósito:
-- ▪ Describe a la persona más vieja de la lista dada.
-- Precondiciones:
-- ▪ La lista posee al menos una persona.
--
elMasViejo :: [Persona] -> Persona
elMasViejo (persona:personas) = elMasViejoEntre persona personas

-- Propósito:
-- ▪ Describe al más viejo entre la persona dada y las personas de la lista dada.
--   En caso de que la lista sea vacía, describe a la primer persona dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
elMasViejoEntre :: Persona -> [Persona] -> Persona 
elMasViejoEntre elMasViejoAlMomento []                 = elMasViejoAlMomento
elMasViejoEntre elMasViejoAlMomento (persona:personas) =
						 				if esMenorQueLaOtra persona elMasViejoAlMomento
						 				then elMasViejoEntre elMasViejoAlMomento personas
						 				else elMasViejoEntre persona personas 

-- 3.
--
data TipoDePokemon =  Agua 
				    | Fuego
				    | Planta
				    deriving Show

type Energia = Int 

type Nombre = String

data Pokemon = MKPokemon TipoDePokemon Energia deriving Show 

data Entrenador = MKEntrenador Nombre [Pokemon]

-- 1)
-- Propósito:
-- ▪ Describe el elemento que le gana al tipo de pokemón dado.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
elementoGanador :: TipoDePokemon -> TipoDePokemon
elementoGanador Agua   = Planta
elementoGanador Fuego  = Agua
elementoGanador Planta = Fuego

-- 2)

instance Eq TipoDePokemon where
	Fuego  == Fuego  = True
	Planta == Planta = True
	Agua   == Agua   = True
	_ == _           = False

-- Propósito:
-- ▪ Indica si el primer pokemón dado le gana el segundo pokemón dado.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
leGanaA :: Pokemon -> Pokemon -> Bool
leGanaA primerPokemon segundoPokemon = tipo primerPokemon == elementoGanador (tipo segundoPokemon) 

-- Función observadora 👀 
-- Propósito:
-- ▪ Describe el tipo del pokemón dado.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
tipo :: Pokemon -> TipoDePokemon
tipo (MKPokemon tipo _) = tipo 

-- 3)
-- Propósito:
-- ▪ Describe al entrenador dado con la lista de pokemóns actualizada con el pokemón dado agregado.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
capturarPokemon :: Pokemon -> Entrenador -> Entrenador
capturarPokemon pokemon (MKEntrenador nombre pokemons) = MKEntrenador nombre (pokemon : pokemons)

-- 4)
-- Propósito:
-- ▪ Describe la cantidad de pokemons que tiene el entrenador dado.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
cantidadDePokemons :: Entrenador -> Int
cantidadDePokemons entrenador = longitud (pokemons entrenador)

-- Propósito:
-- ▪ Describe la lista de pokemons que tiene el entrenador dado.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
pokemons :: Entrenador -> [Pokemon]
pokemons (MKEntrenador _ pokemons) = pokemons


-- 5)
-- Propósito:
-- ▪ Describe la cantidad de pokemons del tipo dado que tiene el entrenador dado.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
cantidadDePokemonsDeTipo :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonsDeTipo tipo entrenador = 
	cantidadDePokemonsDeTipo_EnLaLista tipo (pokemons entrenador)

-- Propósito:
-- ▪ Describe la cantidad de pokemons del tipo dado en la lista de pokemons dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
cantidadDePokemonsDeTipo_EnLaLista :: TipoDePokemon -> [Pokemon] -> Int 
cantidadDePokemonsDeTipo_EnLaLista _ [] = 0
cantidadDePokemonsDeTipo_EnLaLista tipo (pokemon : pokemons) = 
									if es_DeTipo_ pokemon tipo 
										then 1 + cantidadDePokemonsDeTipo_EnLaLista tipo pokemons

										else cantidadDePokemonsDeTipo_EnLaLista tipo pokemons

-- Propósito:
-- ▪ Indica si el pokemon dado es del tipo dado.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
es_DeTipo_ :: Pokemon -> TipoDePokemon -> Bool
es_DeTipo_ pokemon tipoAVerificar = (tipo pokemon) == tipoAVerificar 

-- 6)
-- Propósito:
-- ▪ Indica si el entrenador dado posee un pokemon que le puede ganar al pokemon dado.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
lePuedeGanar :: Entrenador -> Pokemon -> Bool
lePuedeGanar entrenador pokemon = tiene_unPokemonDeTipo_ entrenador (elementoGanador (tipo pokemon))

-- Propósito:
-- ▪ Indica si el entrenador dado tiene un pokemon del tipo dado en su lista de pokemons.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
tiene_unPokemonDeTipo_ :: Entrenador -> TipoDePokemon -> Bool 
tiene_unPokemonDeTipo_ entrenador tipo = enLaLista_HayUnPokemonDeTipo_ (pokemons entrenador) tipo 

-- Propósito:
-- ▪ Indica si en la lista de pokemons dada hay un pokemon del tipo dado.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
enLaLista_HayUnPokemonDeTipo_ :: [Pokemon] -> TipoDePokemon -> Bool 
enLaLista_HayUnPokemonDeTipo_ [] _ = False
enLaLista_HayUnPokemonDeTipo_ (pokemon : pokemons) tipoAVer = 
					es_DeTipo_ pokemon tipoAVer || enLaLista_HayUnPokemonDeTipo_ pokemons tipoAVer

-- 7)
-- Propósito:
-- ▪ Indica si los dos entrenadores dados tienen un pokemon del tipo dado con energía
--   en sus respectivas listas de pokemons.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
puedenPelear :: TipoDePokemon -> Entrenador -> Entrenador -> Bool 
puedenPelear tipo entrenador1 entrenador2 = 
 (tiene_unPokemonDeTipo_ConEnergia entrenador1 tipo) && (tiene_unPokemonDeTipo_ConEnergia entrenador2 tipo) 


-- Propósito:
-- ▪ Indica si el entrenador dado tiene un pokemon del tipo dado con energía.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
tiene_unPokemonDeTipo_ConEnergia :: Entrenador -> TipoDePokemon -> Bool 
tiene_unPokemonDeTipo_ConEnergia entrenador tipoAVer = 
	tieneLaLista_UnPokemonDeTipo_ConEnergia (pokemons entrenador) tipoAVer

-- Propósito:
-- ▪ Indica si la lista de pokemons dada tiene al menos un pokemon del tipo dado con energía para 
--   pelear.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
tieneLaLista_UnPokemonDeTipo_ConEnergia :: [Pokemon] -> TipoDePokemon -> Bool 
tieneLaLista_UnPokemonDeTipo_ConEnergia [] _ = False
tieneLaLista_UnPokemonDeTipo_ConEnergia (pokemon : pokemons) tipoAVer =
			(es_DeTipo_ pokemon tipoAVer) && (energia pokemon > 0) ||
		    tieneLaLista_UnPokemonDeTipo_ConEnergia pokemons tipoAVer 

-- Propósito:
-- ▪ Describe el nivel de energía del pokemon dado.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
energia :: Pokemon -> Energia
energia (MKPokemon _ energia) = energia  

-- 8)
-- Propósito:
-- ▪ Indica si el entrenador dado es un experto.
-- Precondiciones:
-- ▪ No tiene (es una función total).
-- Observaciones:
-- Un entrenador es experto si tiene al menos un pokemón de cada tipo.
--
esExperto :: Entrenador -> Bool 
esExperto entrenador = tiene_unPokemonDeTipo_ entrenador Agua  &&
					   tiene_unPokemonDeTipo_ entrenador Fuego &&
					   tiene_unPokemonDeTipo_ entrenador Planta 

-- 9)
-- Propósito:
-- ▪ Describe una lista con todos los pokemons de cada entrenador de la lista de entrenadores dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
fiestaPokemon :: [Entrenador] -> [Pokemon]
fiestaPokemon []                          = []
fiestaPokemon (entrenador : entrenadores) = pokemons entrenador ++ fiestaPokemon entrenadores

-- Funciones Auxiliares 🐱‍🏍 

-- Propósito:
-- ▪ Describe la longitud de la lista dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + longitud xs 

-- Propósito:
-- ▪ Describe la sumatoria de los números de la lista dada.
-- Precondiciones:
-- ▪ No tiene (es una función total).
--
sumatoria :: [Int] -> Int 
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs 