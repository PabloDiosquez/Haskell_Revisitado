module Clase_11
where 

type Complejo = (Float, Float)

-- EJERCICIOS 👌

-- 1)
-- Propósito:
-- * Describe la parte real del complejo dado.
-- Precondiciones:
-- * No tiene (es una función total).

real :: Complejo -> Float 
real (a, b) = a

-- 2)
-- Propósito:
-- * Describe la parte imaginaria del complejo dado.
-- Precondiciones:
-- * No tiene (es una función total).

imaginaria :: Complejo -> Float 
imaginaria (a, b) = b 

-- 3)
-- Propósito:
-- * Describe el conjugado del complejo dado.
-- Precondiciones:
-- * No tiene (es una función total).

conjugado  :: Complejo -> Complejo
conjugado (a, b) = (a, -b)


-- 4)
-- Propósito:
-- * Describe la suma de los dos complejos dados.
-- Precondiciones:
-- * No tiene (es una función total).

suma :: Complejo -> Complejo -> Complejo
suma (a, b) (c, d) = (a+c, b+d)


-- 5)
-- Propósito:
-- * Describe el producto de los dos complejos dados.
-- Precondiciones:
-- * No tiene (es una función total).

producto :: Complejo -> Complejo -> Complejo
producto (a, b) (c, d) = (a*c - b*d, b*c + a*d)

-- 6)
-- Propósito:
-- * Describe el inverso multiplicativo del complejo dado.
-- Precondiciones:
-- * El complejo dado no debe ser cero.

inverso :: Complejo -> Complejo 
inverso (a, b) = (a / (a^2 + b^2), (-b) / (a^2 + b^2))


-- 7)
-- Propósito:
-- * Describe el cociente entre los dos complejos dados.
-- Precondiciones:
-- * El divisor no debe ser cero.

cociente :: Complejo -> Complejo -> Complejo
cociente z w = producto z (inverso w)

-- 8)
-- Propósito:
-- * Describe el complejo que resulta de elevar a la <<n>> el complejo dado.
-- Precondiciones:
-- * <<n>> debe ser >= 0.

potencia :: Complejo -> Int -> Complejo
potencia z 0 = (1, 0)
potencia z n = producto (potencia z (n-1)) z


-- MÁS EJERCICIOS 💔

-- 1)
-- Propósito:
-- * Describe el módulo del complejo dado.
-- Precondiciones:
-- * No tiene (es una función total).

modulo :: Complejo -> Float 
modulo (a, b) = sqrt(a^2 + b^2) 

-- 2)
-- Propósito:
-- * Describe el argumento del complejo dado.
-- Precondiciones:
-- * El complejo dado no debe ser cero.

argumento :: Complejo -> Float 
argumento (a, b) | a == 0 && b > 0 = 0 					-- Semieje positivo de las x's 
				 | a == 0 && b < 0 = pi 				-- Semieje negativo de las x's
				 | a > 0 && b >= 0 = atan(b/a)   		-- 1er cuadrante
				 | a < 0 && b >= 0 = pi/2 + atan(b/a)   -- 2do cuadrante
				 | a < 0 && b <= 0 = pi   + atan(b/a)   -- 3er cuadrante
				 | a > 0 && b <= 0 = 2*pi - atan(b/a)   -- 4to cuadrante 
				 | otherwise       = undefined 


-- 3) 
-- Propósito:
-- * Describe el complejo que resulta de pasar a coordenadas cartesianas el módulo <<rho>> y 
-- el argumento <<theta>> dados.
-- Precondiciones:
-- * <<rho>> debe ser >= 0.
-- * <<theta>> debe pertenecer al intervalo [0, 2pi).

pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas rho theta = (rho*(cos theta), rho*(sin theta))

-- 4)
-- Propósito:
-- * Dado el complejo z describe los dos complejos w que satisfacen w^2 = z. 
-- Precondiciones:
-- * No tiene (es una función total).

raizCuadrada :: Complejo -> (Complejo, Complejo)
raizCuadrada z = (w, opuesto w) 
			     where w = pasarACartesianas (sqrt(modulo z)) ((argumento z)/2)

-- Propósito:
-- * Describe el opuesto del complejo dado. 
-- Precondiciones:
-- * No tiene (es una función total).

opuesto :: Complejo -> Complejo
opuesto (a, b) = (-a, -b) 


-- EJERCICIOS FINALES 🎳

-- 1)
-- Propósito:
-- * Dado un número natural <<n>>, describe una lista con las raíces n-ésimas de la unidad. 
-- Precondiciones:
-- * <<n>> debe ser >= 0.

raicesNEsimas :: Int -> [Complejo]
raicesNEsimas n = raicesNEsimasDesde 0 n  


-- Propósito:
-- * Describe una lista con las raíces n-ésimas de la unidad desde <<k>>. 
-- Precondiciones:
-- * <<k>> debe ser >= 0 y menor que n.

raicesNEsimasDesde :: Int -> Int -> [Complejo]
raicesNEsimasDesde k n | k >= n     = [] 
					   | otherwise  = kEsimaRaiz : raicesNEsimasDesde (k+1) n 
					   where kEsimaRaiz = pasarACartesianas 1 (2*(fromIntegral k)*pi / (fromIntegral n))