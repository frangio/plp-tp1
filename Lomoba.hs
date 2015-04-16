module Lomoba where
import Grafo
import Tipos
import qualified Data.List as List (delete, union, nub)


-- ---------------------------------Sección 6--------- Lomoba ---------------------------

-- Ejercicio 10
foldExp :: (Prop -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b) -> (b -> b) -> Exp -> b
foldExp fVar fNot fOr fAnd fD fB ei
	= let frec = foldExp fVar fNot fOr fAnd fD fB in
	case ei of
	Var p -> fVar p
	Not e -> fNot (frec e)
	Or e1 e2 -> fOr (frec e1) (frec e2) 
	And e3 e4 -> fAnd (frec e3) (frec e4) 
	D e5 -> fD (frec e5) 
	B e6 -> fB (frec e6)
	
-- Ejercicio 11
-- Revisar: en el caso B(B(VAR "p")), deberia reducirse a 1?
visibilidad :: Exp -> Integer
visibilidad = foldExp fVar fNot fOr fAnd fD fB
	where  
		fVar = const 0
		fNot x = x
		fOr x y = x + y
		fAnd x y = x + y
		fD x = x+1
		fB x = x+1
	

-- Ejercicio 12
extraer :: Exp -> [Prop]
extraer = List.nub . foldExp fVar fNot fOr fAnd fD fB
	where  
		fVar x = [x]
		fNot x = x
		fOr x y = x ++ y
		fAnd x y = x ++ y
		fD x = x
		fB x = x

-- Ejercicio 13
eval :: Modelo -> Mundo -> Exp -> Bool
eval = undefined

-- Ejercicio 14
valeEn :: Exp -> Modelo -> [Mundo]
valeEn = undefined

-- Ejercicio 15
quitar :: Exp -> Modelo -> Modelo
quitar = undefined

-- Ejercicio 16
cierto :: Modelo -> Exp -> Bool
cierto = undefined

