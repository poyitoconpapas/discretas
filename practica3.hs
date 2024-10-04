-- Definicion de estructura
data List a = Void | Node a (List a) deriving Show

--1. Longitud de una lista
longitud :: List a -> Int
longitud Void = 0
longitud (Node _ rest) = 1 + longitud rest

--2. Contención de un elemento en una lista
estaContenido :: (Eq a) => List a -> a -> Bool
estaContenido Void _ = False
estaContenido (Node x rest) elem
    | x == elem = True
    | otherwise = estaContenido rest elem

--3. Convertir una lista de haskell a nuestra nueva estructura ded lista
convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs)

--4. Convertir nuestra nueva estructura en una lista ya definida en haskell
convertirALista :: List a -> [a]
convertirALista Void = []
convertirALista (Node x rest) = x : convertirALista rest

--5. Convertir en un conjunto nuestra nueva estructura de lista
conjunto :: Eq a => List a -> List a
conjunto lista = convertirAEstructura (conjuntoLista (convertirALista lista))

conjuntoLista :: Eq a => [a] -> [a]
conjuntoLista [] = []
conjuntoLista (x:xs) = x : conjuntoLista [y | y <- xs, y /= x]

--6. Eliminar un elemento en un índice específico
eliminarIndice :: List a -> Int -> List a
eliminarIndice Void _ = Void
eliminarIndice (Node x rest) 0 = rest
eliminarIndice (Node x rest) n
    | (n < 0) || (n >= longitud (Node x rest)) = error "Indice fuera del rango permitido."
    | otherwise = Node x (eliminarIndice rest (n - 1))

--7. Agregar un elemento en un índice especifico
insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void _ _ = Void
insertarIndice (Node x rest) 0 elem = Node elem (Node x rest)
insertarIndice (Node x rest) n elem
    | (n < 0) || (n > longitud (Node x rest)-1) = error "Indice fuera del rango permitido."
    | otherwise = Node x (insertarIndice rest (n - 1) elem)

--8. Recorrer n veces a la izquierda los elementos de nuestra nueva estructura de lista
recorrerLista :: List a -> Int -> List a
recorrerLista lista 0 = lista
recorrerLista (Node x xs) n = recorrerLista (concatenar xs (Node x Void)) (n - 1)

concatenar :: List a -> List a -> List a
concatenar Void ys = ys
concatenar (Node x xs) ys = Node x (concatenar xs ys)
