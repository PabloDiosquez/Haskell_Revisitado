-- Operaciones elementales 🐸

-- 1. 
10 - 4 == 7
resta: 1 
comparación ==: 1 
--
total: 2 operaciones elementales 

-- 2. 
if 10 - 4 == 7 
	then 5 
	else 5*5

evaluar la condición: 2 
multiplicar: 1
--
total: 3 operaciones elementales 

-- 3.
[(2*2, 'a'),(3*3, 'b'),(4*4, 'c')] = (2*2, 'a') : ((3*3, 'b') : ((4*4, 'c') : [])) -- : ➡ operador cons

multiplicaciones: 3
construir los pares: 3 
hacer los cons: 3
-- 
total: 9 operaciones elementales

-- 4. 
f [] = 21 + 21
f (x:xs) = (x+x) * f xs 

f [1,2,3] = 
	(1+1) * f [2,3] =
		(1+1) * (2+2) * f [3] = 
			(1+1) * (2+2) * (3+3) * f [] = 
			 	(1+1) * (2+2) * (3+3) * (21 + 21)

construir la lista: 3 
llamar a f por primera vez: 1
llamar a f por segunda vez: 1 
llamar a f por tercera vez: 1 
llamar a f por cuarta vez : 1 
sumas: 4 
multiplicaciones: 3 
-- 
total: 14 operaciones elementales