-- Definición de estructura
data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

--Ejercicio 1
longitud :: Arbol a -> Int
longitud ArbolVacio = 0
longitud (Raiz _ y z) = 1 + longitud (y) + longitud (z)

--Ejercicio 2
profundidad :: Arbol a -> Int
profundidad ArbolVacio = error "El árbol está vacío"
profundidad (Raiz _ y z) = max (profundidad (y)) (profundidad (z)) + 1

--Ejercicio 3
ancho :: Arbol a -> Int
ancho ArbolVacio = 0
ancho (Raiz _ ArbolVacio ArbolVacio) = 1
ancho (Raiz _ y ArbolVacio) = 1
ancho (Raiz _ ArbolVacio z) = 1
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
minimo (Raiz a ArbolVacio ArbolVacio) = a
minimo (Raiz a izq der)
    | a <= minimo(izq) && a <= minimo(der) = a
    | minimo(izq) <= a && minimo(izq) <= minimo(der) = minimo(izq)
    | otherwise = minimo(der)

--Ejercicio 7
maximo :: Ord a => Arbol a -> a
maximo (Raiz x ArbolVacio ArbolVacio) = x
maximo (Raiz x y z)
    | x >= maximo(y) && x >= maximo(z) = x
    | maximo(y) >= x && maximo(y) >= maximo(z) = maximo(y)
    | otherwise = maximo(z)

--Ejercicio 8
eliminar :: Ord a => Arbol a -> a -> Arbol a
eliminar ArbolVacio elemento = error "No puedes eliminar algo de un árbol vacío"
eliminar (Raiz x ArbolVacio ArbolVacio) elemento
    | x == elemento = ArbolVacio
    | otherwise = error "El elemento a eliminar no se encuentra dentro del árbol"

eliminar (Raiz x y ArbolVacio) elemento
    | elemento == x = y
    | elemento /= x = Raiz x (eliminar y elemento) ArbolVacio
    |otherwise = error "El elemento a eliminar no se encuentra dentro del árbol"

eliminar (Raiz x ArbolVacio z) elemento
    | elemento == x = z
    | elemento /= x = Raiz x ArbolVacio (eliminar z elemento)
    | otherwise = error "El elemento a eliminar no se encuentra dentro del árbol"

eliminar (Raiz x y z) elemento
    | elemento == x = Raiz (minimo z) y (eliminar z (minimo z))
    | elemento /= x = Raiz x y (eliminar z elemento)
    | otherwise = Raiz x (eliminar y elemento) z
