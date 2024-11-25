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
conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x:conjunto[y | y <- xs, y /= x]

variables :: Formula -> [Var]
variables (Atom var) = [var]
variables (Neg formula) = variables formula
variables (f1 :&: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :|: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :=>: f2) = conjunto (variables f1 ++ variables f2)
variables (f1 :<=>: f2) = conjunto (variables f1 ++ variables f2)
-----------------------------------------------------

-------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion (Atom var) = Neg var 
negacion (Neg (Atom var)) = Atom var
negacion (f1 :&: f2) = negacion f1 :|: negacion f2
negacion (f1 :|: f2) = negacion f1 :&: negacion f2
negacion (f1 :=>: f2) = f1 :&: negacion f2 
negacion (f1 :<=>: f2) = negacion (f1 :=>: f2) :&: negacion (f2 :=>: f1) 
-----------------------------------------------------

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom var) = Atom var
equivalencia (Neg f1) = negacion (equivalencia f1)
equivalencia (f1 :&: f2) = equivalencia f1 :&: equivalencia f2
equivalencia (f1 :|: f2) = equivalencia f1 :|: equivalencia f2
equivalencia (f1 :=>: f2) = negacion (equivalencia f1) :|: (equivalencia f2) 
equivalencia (f1 :<=>: f2) = equivalencia (f1 :=>: f2) :&: equivalencia (f2 :=>: f1)
-----------------------------------------------------

-------------------- EJERCICIO 4 --------------------
interpretacion :: Formula -> [(Var, Bool)] -> Bool
interpretacion (Atom v) estado = buscarValor v estado
interpretacion (Neg f) estado = not (interpretacion f estado)
interpretacion (f1 :&: f2) estado = interpretacion f1 estado && interpretacion f2 estado
interpretacion (f1 :|: f2) estado = interpretacion f1 estado || interpretacion f2 estado
interpretacion (f1 :=>: f2) estado = not (interpretacion f1 estado) || interpretacion f2 estado
interpretacion (f1 :<=>: f2) estado = interpretacion f1 estado == interpretacion f2 estado

buscarValor :: Var -> [(Var, Bool)] -> Bool
buscarValor v [] = error "No todas las variables están definidas"
buscarValor v ((clave, valor):resto) =
  if v == clave
    then valor
    else buscarValor v resto
-----------------------------------------------------

-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var, Bool)]]
combinaciones (Atom v) = [[(v, False)], [(v, True)]]
combinaciones (f1 :&: f2) = concatenar (combinaciones f1) (combinaciones f2)
combinaciones (f1 :|: f2) = concatenar (combinaciones f1) (combinaciones f2)
combinaciones (f1 :=>: f2) = concatenar (combinaciones f1) (combinaciones f2)
combinaciones (f1 :<=>: f2) = concatenar (combinaciones f1) (combinaciones f2)

concatenar :: [[(Var, Bool)]] -> [[(Var, Bool)]] -> [[(Var, Bool)]]
concatenar [] _ = []
concatenar (x:xs) ys = concatenarAux x ys ++ concatenar xs ys

concatenarAux :: [(Var, Bool)] -> [[(Var, Bool)]] -> [[(Var, Bool)]]
concatenarAux _ [] = []
concatenarAux x (y:ys) = (x ++ y) : concatenarAux x ys 
-----------------------------------------------------

-------------------- EJERCICIO 6 --------------------

tablaDeVerdad :: Formula -> [([(Var, Bool)], Bool)]
tablaDeVerdad f = [(comb, interpretacion f comb) | comb <- combinaciones f]
-----------------------------------------------------


