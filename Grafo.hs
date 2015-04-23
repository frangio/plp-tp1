module Grafo (Grafo, vacio, nodos, vecinos, agNodo, sacarNodo, agEje, lineal, union, clausura) where

import qualified Data.List as List (delete, union, nub)

-- Invariante: ninguna de las listas (nodos y vecinos de nodos) tienen repetidos
data Grafo a = G [a] (a -> [a])

instance (Show a) => Show (Grafo a) where
	show (G n e) = "[\n" ++ concat (map (\x -> " " ++ show x ++ " -> " ++ show (e x) ++ "\n") n) ++ "]"


-- ---------------------------------Sección 3--------- Grafos ---------------------------

-- Ejercicio 1
-- Crea un grafo sin nodos.
vacio :: Grafo a
vacio = G [] (\_ -> [])

-- Ejercicio 2
-- Devuelve los nodos de un grafo.
nodos :: Grafo a -> [a]
nodos (G ns fv) = ns

-- Ejercicio 3
-- Devuelve los vecinos de un nodo en un grafo dado.
vecinos :: Grafo a -> a -> [a]
vecinos (G ns fv) = fv

-- Ejercicio 4
-- Devuelve un grafo con un nodo agregado, sin vecinos.
-- Si el nodo ya está presente en el grafo, no tendrá vecinos en nuevo grafo.
agNodo :: Eq a => a -> Grafo a -> Grafo a
agNodo n (G ns fv) = G (List.union [n] ns) (\m -> if n == m then [] else fv m)

-- Ejercicio 5
-- Devuelve un grafo sin un nodo dado.
sacarNodo :: Eq a => a -> Grafo a -> Grafo a
sacarNodo n (G ns fv) = G (List.delete n ns) (\e -> if e == n  then [] else (List.delete n.fv) e)

-- Ejercicio 6
-- Devuelve un grafo con un eje agregado.
agEje :: Eq a =>  (a,a) -> Grafo a -> Grafo a
agEje (n1, n2) (G ns fv) = G ns (\e -> if e == n1 then (n2:fv e) else (fv e)) 

-- Ejercicio 7
-- Devuelve un grafo donde los nodos son todos los de la lista pasada por argumento, y cada nodo tiene como unico vecino al elemento que lo sigue en dicha lista.
lineal :: Eq a => [a] -> Grafo a
lineal ns = G ns (\n -> take 1 $ drop 1 $ dropWhile (/= n) ns) 

-- Ejercicio 8
-- Devuelve la unión de dos grafos.
union :: Eq a => Grafo a -> Grafo a -> Grafo a
union (G ns1 fv1) (G ns2 fv2) = G (List.union ns1 ns2) (\e -> (List.union (fv1 e) (fv2 e)))

-- Ejercicio 9
-- Devuelve la clausura reflexo transitiva de un grafo.
clausura :: (Eq a) => Grafo a -> Grafo a
clausura (G ns fv) = G ns (\x -> puntofijo extenderConVecinos [x])
  where extenderConVecinos xs = foldl List.union xs (map fv xs)

puntofijo :: (Eq a) => (a -> a) -> (a -> a)
puntofijo f x = head $ dropWhile (\e -> (f e) /= e) $ iterate f x

