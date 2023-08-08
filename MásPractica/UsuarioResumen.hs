module UsuarioR where 

import Resumen 

-- Ejemplo de uso del TAD Resumen 🤔 
-- 
temperaturas :: Resumen 
temperaturas = 
	agregarR 19 (agregarR 22 (agregarR 20 (agregarR 12 nuevoR))) 