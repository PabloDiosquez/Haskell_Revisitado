module Stack where 

-- Interfaz del módulo Stack 🐸 

-- Crea una pila vacía 🔋
emptyS :: Stack a 

-- Indica si la pila dada está vacía.
isEmptyS :: Stack a -> Bool  

-- Describe actualizada agregando el elemento dado a la pila dada.
push :: a -> Stack a -> Stack a  

-- Describe el elemento del tope de la pila dada.
tope :: Stack a -> a 

-- Describe la pila dada sin el primer elemento.
pop :: Stack a -> Stack a 

-- 1. Como usuario del TAD Stack implementar las siguientes funciones: 
-- ▪ 
-- Dada una lista describe una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a 
apilar xs = apilar' xs emptyS

apilar' :: [a] -> Stack a -> Stack a 
apilar' (x:xs) s = 
	apilar' xs (push x s) s

-- ▪ 
-- Toma un string que representa una expresión aritmética, por ejemplo ”(2 + 3) ∗ 2”, y verifica que la
-- cantidad de paréntesis que abren se corresponda con los que cierran.
balanceado :: String -> Bool 
balanceado cs = balanceado' cs emptyS

balanceado' :: String -> Stack a -> Bool 
balanceado' [] s     = isEmptyS s 
balanceado' (c:cs) s = 
	if c == "("
		then push c s 
		else if not (isEmptyS s) && (c == ")")
				then pop s 
				else balanceado' cs s      