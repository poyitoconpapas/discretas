longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento [] elem condicion = [elem]
agregaElemento lista elem condicion =
    if condicion == True
        then elem : lista
        else lista ++ [elem]

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = error "La lista no puede estar vacía"
maximoLista [x] = x
maximoLista (x:xs) =
    if x > maximoLista xs
        then x
        else maximoLista xs

indice :: [a] -> Int -> a
indice (x:xs) 0 = x
indice (x:xs) i =
    if i > longitud (x:xs) - 1 || i <= 0
        then error "índice fuera de rango"
        else indice xs (i-1)

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [y | y <- xs, y /= x]

numerosPares :: [Int] -> [Int]
numerosPares [] = []
numerosPares xs = [x | x <- xs, x `mod` 2 == 0]
