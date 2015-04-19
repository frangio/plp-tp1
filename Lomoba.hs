module Lomoba where
import Grafo
import Tipos
import qualified Data.List as List (delete, union, nub)


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
extraer = List.nub . foldExp fVar fNot fOr fAnd fD fB
  where fVar = (:[])
        fNot = id
        fOr = (++)
        fAnd = (++)
        fD = id
        fB = id

-- Ejercicio 13
eval :: Modelo -> Mundo -> Exp -> Bool
eval m@(K g v) w e = foldExp fVar fNot fOr fAnd fD fB e
  where fVar x = elem w (v x)
        fNot = not
        fOr = (||)
        fAnd = (&&)
        fD _ = any (\w' -> eval m w' e) (vecinos g w)
        fB _ = all (\w' -> eval m w' e) (vecinos g w)

-- Ejercicio 14
--duda acá : tengo que devolver los mundos del modelo para los que vale la fórmula?
valeEn :: Exp -> Modelo -> [Mundo]
valeEn e m@(K g fv) = filter (\x -> eval m x e) (nodos g)

-- Ejercicio 15
-- no se si está bien. yo no cambie fv, pero lo cierto es que los nodos que saqué ya no están en el grafo, y por lo tanto tampoco en el modelo
quitar :: Exp -> Modelo -> Modelo
quitar e m@(K g fv) = K (foldl (flip sacarNodo) g (noValeEn e m)) fv

noValeEn :: Exp -> Modelo ->[Mundo]
noValeEn e m@(K g fv) = filter (\x -> not (eval m x e)) (nodos g)

-- Ejercicio 16
cierto :: Modelo -> Exp -> Bool
cierto m@(K g fv) e = foldl (&&) True (map(\x -> eval m x e) (nodos g))

--sacarNodos g ls = foldl (flip sacarNodo) g ls
