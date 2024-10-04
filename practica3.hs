data List a = Void | Node a (List a) deriving Show

longitud :: List a -> Int
longitud Void = 0
longitud (Node _ rest) = 1 + longitud rest

estaContenido :: (Eq a) => List a -> a -> Bool
estaContenido Void _ = False
estaContenido (Node x rest) elem
    | x == elem = True
    | otherwise = estaContenido rest elem

convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs)

convertirALista :: List a -> [a]
convertirALista Void = []
convertirALista (Node x rest) = x : convertirALista rest

conjunto :: Eq a => List a -> List a
conjunto lista = convertirAEstructura (conjuntoLista (convertirALista lista))

conjuntoLista :: Eq a => [a] -> [a]
conjuntoLista [] = []
conjuntoLista (x:xs) = x : conjuntoLista [y | y <- xs, y /= x]

eliminarIndice :: List a -> Int -> List a
eliminarIndice Void _ = Void
eliminarIndice (Node x rest) 0 = rest
eliminarIndice (Node x rest) n =
    if (n < 0) || (n >= longitud (Node x rest)-1)
       then error "Indice fuera del rango permitido."
       else Node x (eliminarIndice rest (n - 1))

insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void _ _ = Void
insertarIndice (Node x rest) 0 elem = Node elem (Node x rest)
insertarIndice (Node x rest) n elem =
    if (n < 0) || (n > longitud (Node x rest)-1)
       then error "Indice fuera del rango permitido."
       else
        Node x (insertarIndice rest (n - 1) elem)



