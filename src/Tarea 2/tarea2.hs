{-
Facultad de Ciencias UNAM - Programación Declarativa 2018-2
Profesor: C. Moisés Vázquez Reyes
Ayudante: Enrique Antonio Bernal Cedillo
-}

module Tarea2 where

import Test.QuickCheck

-- | Los átomos son cadenas.
type At = String
-- | Fórmulas de la lógica proposicional en forma normal negativa.
data F = Var At | Neg At | Conj F F | Disy F F deriving Eq

-- | Para pintar fórmulas de forma especial.
instance Show F where
   show f = case f of
             Var p      -> p
             Neg p      -> "¬"++p
             Conj f1 f2 -> "(" ++ show f1 ++ "∧" ++ show f2 ++ ")"
             Disy f1 f2 -> "(" ++ show f1 ++ "∨" ++ show f2 ++ ")"

-- | Para usar QuickCheck sobre F
instance Arbitrary F where
  arbitrary
    = frequency [(2, Var <$> arbitrary),
                 (2, Neg <$> arbitrary),
                 (1, do f1 <- arbitrary
                        f2 <- arbitrary
                        return (Conj f1 f2)),
                 (1, do f1 <- arbitrary
                        f2 <- arbitrary
                        return (Disy f1 f2))]

-- | La implicación es un caso particular de la disyunción.
imp :: F -> F-> F
imp f = Disy (neg f)

-- | Para negar fórmulas en general.
-- prop> neg (neg  f) == f
neg :: F -> F
neg f = case f of
          Var p -> Neg p
          Neg p -> Var p
          Conj f1 f2 -> Disy (neg f1) (neg f2)
          Disy f1 f2 -> Conj (neg f1) (neg f2)

{-===============================    ment themselvesTimes :: [Int] -> [Int], which takes each number n {\displaystyle n} n in the argument list and generates n {\displaystyle n} n copies of it in the result list.

Board game example

Suppose we are modeling a turn-based board game and want to find all the possible ways the game could progress. We would need a function to calculate the list of options for the next turn, given a current board state:

nextConfigs :: Board -> [Board]
nextConfigs bd = undefined -- details not important

To figure out all the possibilities after two turns, we would again apply our function to each of the elements of our new list of board states. Our function takes a single board state and returns a list of possible new states. Thus, we can use monadic binding to map the function over each element from the list:

nextConfigs bd >>= nextConfigs
==============================-}
--Una literal es una fórmula atómica o la negación de una fórmula atómica.
--Se considera que una 'Literal' únicamente es de la forma 'Var _' o 'Neg _'.
type Literal = F
--Una cláusula es una literal o una disyunción de literales.
--La lista [l1,l2,..,lk] significa (l1 ⋁ l2 ⋁..⋁ lk)
type Clausula = [Literal]

soloConj :: F -> Bool
soloConj f = case f of
               (Conj f1 f2) -> soloConj f1 && soloConj f2
               (Disy _  _ ) -> False
               _            -> True

soloDisy :: F -> Bool
soloDisy f = case f of
               (Disy f1 f2) -> soloDisy f1 && soloDisy f2
               (Conj _  _)  -> False
               _            -> True

-- | Regresa una lista de Literales si una Formula es aplaztable (solo conj o solo disy)
-- >>> aplaztar $ Disy (Disy (Var "x") (Var "y")) (Var "z")
-- Just [x,y,z]
-- >>> aplaztar $ Conj (Disy (Var "x") (Var "y")) (Var "z")
-- Nothing
aplaztar :: F -> Maybe [F]
aplaztar f = case f of
               (Var v) -> Just [Var v]
               (Neg v) -> Just [Neg v]
               (Conj f1 f2) -> if soloConj f1 && soloConj f2
                               then continuar f1 f2
                               else Nothing
               (Disy f1 f2) -> if soloDisy f1 && soloDisy f2
                               then continuar f1 f2
                               else Nothing
  where continuar f1 f2 = do f1' <- aplaztar f1
                             f2' <- aplaztar f2
                             return $ f1' ++ f2'

-- | Conjunta una lista de formulas
conjuntar :: [F] -> F
conjuntar l = case l of
                []   -> undefined
                [f]  -> f
                f:xs -> Conj f $ conjuntar xs

-- | Disyunta una lista de formulas
disyuntar :: [F] -> F
disyuntar l = case l of
                []   -> undefined
                [f]  -> f
                f:xs -> Disy f $ disyuntar xs

-- | Transforma una fórmula a FNC.
fnc :: F -> F
fnc (Conj f1 f2) | soloConj f1 || soloDisy f1 = Conj f1 $ fnc f2
                 | soloConj f2 || soloDisy f2 = Conj f2 $ fnc f1
                 | otherwise = undefined
fnc (Disy f1 f2) | soloDisy f1 && soloDisy f2 = Disy f1 f2
                 | otherwise = undefined
fnc f = case f of
          Var p -> Var p
          Neg p -> Neg p
          _     -> undefined

--Obtiene las cláusulas de una fórmula.
clausulas::F->[Clausula]
clausulas = error "Te toca"

--Realiza el algoritmo DPLL y pinta en pantalla el árbol generado por la ejecución,
--y en cada nivel se indica la operación realizada.
dpll = error "Te toca"



ejer1_1 = dpll $ concatMap clausulas $ [Disy (Var "a") (Var "b"),imp (neg $ Var "c") (neg $ Var "a")]++[neg $ imp (Var "b") (neg $ Var "c")]
ejer1_2 = error "Te toca"
ejer1_3 = error "Te toca"
ejer1_4 = error "Te toca"


ejer2_a = error "Te toca"
ejer2_b = error "Te toca"




