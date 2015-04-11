module Grafo (Grafo, vacio, nodos, vecinos, agNodo, sacarNodo, agEje, lineal, union, clausura) where

data Grafo a = G [a] (a -> [a])

instance (Show a) => Show (Grafo a) where
	show (G n e) = "[\n" ++ concat (map (\x -> " " ++ show x ++ " -> " ++ show (e x) ++ "\n") n) ++ "]"


-- ---------------------------------Sección 3--------- Grafos ---------------------------

-- Ejercicio 1
-- Crea nu Grafo con 
--		- ningun nodo ([]),
--		- una funcion de vecindad que a cualquier posible nodo asocia una lista vacia de vecinos (\_ -> [])
vacio :: Grafo a
vacio = G [] (\_ -> [])

-- Ejercicio 2
nodos :: Grafo a -> [a]
nodos (G ns fv) = ns

-- Ejercicio 3
vecinos :: Grafo a -> a -> [a]
vecinos (G ns fv) = fv

-- Ejercicio 4
agNodo :: Eq a => a -> Grafo a -> Grafo a
agNodo n (G ns fv) 	
	| n `elem` ns 	= 	(G ns fv)
	| otherwise 	=	(G (n:ns) fv)

-- Ejercicio 5
sacarNodo :: Eq a => a -> Grafo a -> Grafo a
sacarNodo n (G ns fv)	
	= 	(G (removerElem ns) (\e -> removerElem (fv e))) --quitar los de ns
		where removerElem =  (filter (\e -> e /= n))

-- Ejercicio 6
agEje :: Eq a =>  (a,a) -> Grafo a -> Grafo a
agEje (n1,n2) (G ns fv) = (G ns (\e -> if e == n1 then n2:(fv e) else fv e) )

-- Ejercicio 7
lineal :: Eq a =>  [a] -> Grafo a
lineal ns = (G ns (\n ->  cabeza ( tail ( dropWhile (/= n) ns ) )  ) )

cabeza [] = []
cabeza (x:xs) = [x]

-- Ejercicio 8
union :: Eq a => Grafo a -> Grafo a -> Grafo a
union (G ns1 fv1) (G ns2 fv2) 
	= ( G (sinRepetidos (ns1 ++ ns2)) (\e -> (sinRepetidos (fv1 e) ++ (fv2 e) ) ) )
	where sinRepetidos = foldr (\n rec -> if n `notElem` rec then n:rec else rec) []
	
-- Ejercicio 9
clausura :: Grafo a -> Grafo a
clausura = undefined





