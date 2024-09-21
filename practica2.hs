longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento lista elem True = elem : lista
agregaElemento lista elem False = lista ++ [elem]

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [x] = x
maximoLista (x:xs) = max x (maximoLista xs)

indice :: [a] -> Int -> a
indice lista i =
    if i > longitud lista - 1
   then error "índice fuera de rango"
   else indicevalido lista i
    where
        indicevalido (x:xs) 0 = x
        indicevalido (x:xs) i = indicevalido xs (i - 1)

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = if x `elem` xs
   then conjunto xs
   else x : conjunto xs


numerosPares :: [Int] -> [Int]
numerosPares [] = []
numerosPares xs = [x | x <- xs, x `mod` 2 == 0]
