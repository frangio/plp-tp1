module Lomoba where
import Grafo
import Tipos
import qualified Data.List as List (union, intersect)


-- ---------------------------------Sección 6--------- Lomoba ---------------------------

-- Ejercicio 10
foldExp :: (Prop -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b) -> (b -> b) -> Exp -> b
foldExp fVar fNot fOr fAnd fD fB ei =
  let frec = foldExp fVar fNot fOr fAnd fD fB
  in case ei of
    Var p -> fVar p
    Not e -> fNot (frec e)
    Or e1 e2 -> fOr (frec e1) (frec e2)
    And e3 e4 -> fAnd (frec e3) (frec e4)
    D e5 -> fD (frec e5)
    B e6 -> fB (frec e6)

-- Ejercicio 11
visibilidad :: Exp -> Integer
visibilidad = foldExp fVar fNot fOr fAnd fD fB
  where fVar = const 0
        fNot = id
        fOr = max
        fAnd = max
        fD = (1+)
        fB = (1+)

-- Ejercicio 12
extraer :: Exp -> [Prop]
extraer = foldExp fVar fNot fOr fAnd fD fB
  where fVar p = [p]
        fNot = id
        fOr = List.union
        fAnd = List.union
        fD = id
        fB = id

-- Ejercicio 13
eval :: Modelo -> Mundo -> Exp -> Bool
eval m@(K g v) w e = foldExp fVar fNot fOr fAnd fD fB e
  where fVar x = elem w (v x)
        fNot = not
        fOr = (||)
        fAnd = (&&)
        fD _ = any (eval' m e) (vecinos g w)
        fB _ = all (eval' m e) (vecinos g w)

eval' m e w = eval m w e

-- Ejercicio 14
valeEn :: Exp -> Modelo -> [Mundo]
valeEn e m@(K g v) = filter (eval' m e) (nodos g)

-- Ejercicio 15
quitar :: Exp -> Modelo -> Modelo
quitar e m@(K g v) = K g' v'
  where g' = foldl (flip sacarNodo) g (noValeEn e m)
        v' p = List.intersect (nodos g') (v p)

noValeEn :: Exp -> Modelo ->[Mundo]
noValeEn e m@(K g v) = filter (not . (eval' m e)) (nodos g)

-- Ejercicio 16
cierto :: Modelo -> Exp -> Bool
cierto m@(K g v) e = all (eval' m e) (nodos g)
