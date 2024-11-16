-- Definición de estructura
data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

--Ejercicio 1
longitud :: Arbol a -> Int
longitud ArbolVacio = 0
longitud (Raiz _ y z) = 1 + longitud (y) + longitud (z)

--Ejercicio 2
profundidad :: Arbol a -> Int
profundidad ArbolVacio = 0
profundidad (Raiz _ y z) = max (profundidad (y)) (profundidad (z)) + 1

--Ejercicio 3
ancho :: Arbol a -> Int
ancho ArbolVacio = 0
ancho (Raiz _ ArbolVacio ArbolVacio) = 1
ancho (Raiz _ y z) = ancho (y) + ancho (z)

--Ejercicio 4
data Recorrido = InOrder | PreOrder | PostOrder deriving Show

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio _ = []
recorrido (Raiz x y z) InOrder = recorrido y InOrder ++ [x] ++ recorrido z InOrder
recorrido (Raiz x y z) PreOrder = [x] ++ recorrido y PreOrder ++ recorrido z PreOrder
recorrido (Raiz x y z) PostOrder = recorrido y PostOrder ++ recorrido z PostOrder ++ [x]

--Ejercicio 5
niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz x y z) = [x] : combinar (niveles y) (niveles z)
    where
        combinar [] ys = ys
        combinar xs [] = xs
        combinar (x:xs) (y:ys) = (x ++ y) : combinar xs ys

--Ejercicio 6
minimo :: Ord a => Arbol a -> a
minimo (ArbolVacio) = error "Un árbol vacío no tiene minimo"
minimo (Raiz x ArbolVacio ArbolVacio) = x
minimo (Raiz x y ArbolVacio)
    | x <= minimo y = x
    | otherwise = minimo y
minimo (Raiz x ArbolVacio z)
    | x <= minimo z = x
    | otherwise = minimo z
minimo (Raiz x y z)
    | x <= minimo(y) && x < minimo(z) = x
    | minimo(y) <= x && minimo(y) <= minimo(z) = minimo(y)
    | otherwise = minimo(z)

--Ejercicio 7
maximo :: Ord a => Arbol a -> a
maximo (ArbolVacio) = error "Un árbol vacío no tiene máximo"
maximo (Raiz x ArbolVacio ArbolVacio) = x
maximo (Raiz x y ArbolVacio)
    | x >= maximo y = x
    | otherwise = maximo y
maximo (Raiz x ArbolVacio z)
    | x >= maximo z = x
    | otherwise = maximo z
maximo (Raiz x y z)
    | x >= maximo(y) && x >= maximo(z) = x
    | maximo(y) >= x && maximo(y) >= maximo(z) = maximo(y)
    | otherwise = maximo (z)

--Ejercicio 8
eliminar :: Ord a => Arbol a -> a -> Arbol a
eliminar ArbolVacio _ = error "No existe el elemento en el árbol"
eliminar (Raiz x ArbolVacio z) elemento
    | x == elemento = z  -- Devuelve el subárbol derecho
eliminar (Raiz x y ArbolVacio) elemento
    | x == elemento = y  -- Devuelve el subárbol izquierdo
eliminar (Raiz x y z) elemento
    | elemento < x = Raiz x (eliminar y elemento) z  -- Busca en el subárbol izquierdo
    | elemento > x = Raiz x y (eliminar z elemento)  -- Busca en el subárbol derecho
    | otherwise = Raiz (minimo z) y (eliminar z (minimo z))
