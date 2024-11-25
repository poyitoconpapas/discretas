data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Atom Var
    |Neg Formula
    |Formula :&: Formula
    |Formula :|: Formula
    |Formula :=>: Formula
    |Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

-------------------- EJERCICIO 1 --------------------
variables :: Formula -> [Var]
variables (Atom elem) = [elem]
variables (Neg formula) = conjunto (variables formula)
variables (formula1 :&: formula2) = conjunto (variables formula1 ++ variables formula2)
variables (formula1 :|: formula2) = conjunto (variables formula1 ++ variables formula2)
variables (formula1 :=>: formula2) = conjunto (variables formula1 ++ variables formula2)
variables (formula1 :<=>: formula2) = conjunto (variables formula1 ++ variables formula2)

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x : xs) = x : conjunto[y | y <- xs, y /= x]

-------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion (Atom elem) = Neg (Atom elem)
negacion (Neg formula) = formula
negacion (formula1 :&: formula2) = negacion formula1 :|: negacion formula2
negacion (formula1 :|: formula2) = negacion formula1 :&: negacion formula2
negacion (formula1 :=>: formula2) = formula1 :&: negacion formula2
negacion (formula1 :<=>: formula2) = (formula1 :&: negacion formula2) :|: (negacion formula1 :&: formula2)

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom elem) = Atom elem
equivalencia (Neg (formula)) = (negacion formula)
equivalencia (formula1 :&: formula2) = (equivalencia formula1) :&: (equivalencia formula2)
equivalencia (formula1 :|: formula2) = (equivalencia formula1) :|: (equivalencia formula2)
equivalencia (formula1 :=>: formula2) = (negacion formula1) :|: (equivalencia formula2)
equivalencia (formula1 :<=>: formula2) = equivalencia (formula1 :=>: formula2) :&: (equivalencia (formula2 :=>: formula1))


-------------------- EJERCICIO 4 --------------------
interpretacion :: Formula -> [(Var,Bool)] -> Bool
interpretacion (Atom var) valores = valoresAux var valores
interpretacion (Neg formula) valores = not (interpretacion formula valores)
interpretacion (formula1 :&: formula2) valores = interpretacion formula1 valores && interpretacion formula2 valores
interpretacion (formula1 :|: formula2) valores = interpretacion formula1 valores || interpretacion formula2 valores
interpretacion (formula1 :=>: formula2) valores = not (interpretacion formula1 valores) || interpretacion formula2 valores
interpretacion (formula1 :<=>: formula2) valores = interpretacion formula1 valores == interpretacion formula2 valores

valoresAux :: Var -> [(Var, Bool)] -> Bool
valoresAux var [] = error "No todas variables están definidas"
valoresAux var ((v,valor):xs)
    | var == v = valor
    | otherwise = valoresAux var xs

-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones formula
    | null (conjunto (variables formula)) = [[]]
    | otherwise = combinar (conjunto (variables formula))

combinar :: [Var] -> [[(Var,Bool)]]
combinar [] = [[]]
combinar (x:xs) = [(x,True) : y | y <- combinar xs] ++ [(x,False) : y | y <- combinar xs]

-------------------- EJERCICIO 6 --------------------
tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaDeVerdad formula = [(estados, interpretacion formula estados) | estados <- combinaciones formula]
