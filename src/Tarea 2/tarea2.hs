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
    = frequency [(3, Var <$> (:[]) <$> elements ['x'..'z']),
                 (3, Neg <$> (:[]) <$> elements ['x'..'z']),
                 (2, do f1 <- arbitrary
                        f2 <- arbitrary
                        return (Conj f1 f2)),
                 (2, do f1 <- arbitrary
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

{-=============================================================-}
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

bajaDisy :: F -> F
bajaDisy f = case f of
               (Disy (Conj f1 f2) f3) -> Conj (Disy f1 f3) (Disy f2 f3)
               (Disy f1 (Conj f2 f3)) -> Conj (Disy f1 f2) (Disy f1 f3)
               (Conj f1 f2) -> Conj (bajaDisy f1) (bajaDisy f2)
               (Disy f1 f2) -> Disy (bajaDisy f1) (bajaDisy f2)
               (Var v) -> Var v
               (Neg v) -> Neg v

-- | Transforma una fórmula a FNC.
-- prop> fnc (fnc f) == fnc f
--
-- >>> fnc $ Disy (Conj (Var "x") (Var "y")) (Var "z")
-- ((x∨z)∧(y∨z))
fnc :: F -> F
fnc (Conj f1 f2) | soloConj f1 || soloDisy f1 = Conj f1 $ fnc f2
                 | soloConj f2 || soloDisy f2 = Conj f2 $ fnc f1
                 | otherwise = case (f1, f2) of
                                 (Conj f1' f1'', _) -> Conj (Conj (fnc f1') (fnc f1'')) $ fnc f2
                                 (_, Conj f2' f2'') -> Conj (fnc f1) $ Conj (fnc f2') (fnc f2'')
                                 _ -> fnc . bajaDisy $ Conj f1 f2
fnc (Disy f1 f2) | soloDisy f1 && soloDisy f2 = Disy f1 f2
                 | otherwise = fnc . bajaDisy $ Disy f1 f2
fnc (Var v) = Var v
fnc (Neg v) = Neg v

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

--Obtiene las cláusulas de una fórmula.
clausulas :: F -> [Clausula]
clausulas f = case f of
                (Conj f1 f2) -> clausulas f1 ++ clausulas f2
                _ -> case aplaztar f of
                       Just f' -> [f']
                       Nothing -> error "No es una FNC"

data EvalDPLL = PFinal [Literal] Bool | PInter String [Literal] [Clausula] (Maybe EvalDPLL) deriving Eq

instance Show EvalDPLL where
  show (PFinal l b) = if b
                      then "SAT: " ++ show l
                      else "INSAT!"
  show (PInter op l c cont) =
    op ++
    "\nLiterales: " ++ show l ++
    "\nClausulas: " ++ show c ++
    "\n" ++
    "-----\n" ++
    "\n" ++
    case cont of
      Just e  -> show e
      Nothing -> "DPLL Incompleto"

-- | Realiza el algoritmo DPLL y pinta en pantalla el árbol generado por la ejecución,
--   y en cada nivel se indica la operación realizada.
--
-- >>> dpll $ clausulas $ fnc $ Conj (Var "x") (Neg "x")
-- ...
-- INSAT!
-- >>> dpll $ clausulas $ fnc $ Conj (Neg "y") (Disy (Conj (Conj (Neg "z") (Neg "x")) (Var "z")) (Neg "y"))
-- ...
-- SAT: [¬y]
dpll :: [Clausula] -> EvalDPLL
dpll c = dpll' $ PInter "Inicio" [] c Nothing

dpll' :: EvalDPLL -> EvalDPLL
dpll' (PFinal l b) = PFinal l b
dpll' anterior | null c     = siguiente . return $ PFinal l True
               | any null c = siguiente . return $ PFinal l False
               | any (\c' -> length c' == 1) c =
                   siguiente . return $
                   dpll' $ PInter ("Propagación unitaria de: " ++ show primerUnit) (primerUnit:l) cSinUnit Nothing
               | not . null $ puros =
                   siguiente . return $
                   dpll' $ PInter ("Levantando literales puras: " ++ show puros) (puros ++ l) cSinPuros Nothing
               | otherwise = siguiente $ Nothing
  where
    (PInter op l c e) = anterior
    siguiente  = PInter op l c
    primerUnit = head . head $ filter (\c' -> length c' == 1) c
    cSinUnit   = quitarL primerUnit $
                 filter (\c' -> primerUnit `notElem` c') $
                 filter (\c' -> c' /= [primerUnit]) c
    puros      = concat $ map (filter (\c' -> c' `noContradice` c)) c
    cSinPuros  = filter (not . null) $ map (filter (\c' -> c' `notElem` puros)) c

noContradice :: Literal -> [Clausula] -> Bool
noContradice l [] = True
noContradice l (c:cs) = case c of
                          []      -> noContradice l cs
                          (l':ls) -> case (l, l') of
                                       ((Var v), (Neg v')) -> if v == v'
                                                              then False
                                                              else noContradice l ((ls):cs)
                                       ((Neg v), (Var v')) -> if v == v'
                                                              then False
                                                              else noContradice l ((ls):cs)
                                       _ -> noContradice l ((ls):cs)

quitarL :: Literal -> [Clausula] -> [Clausula]
quitarL (Var v) = map (filter (\c' -> c' /= (Var v) && c' /= (Neg v)))
quitarL (Neg v) = map (filter (\c' -> c' /= (Var v) && c' /= (Neg v)))
quitarL _ = undefined

ejer1_1 = dpll $ concatMap clausulas $ [Disy (Var "a") (Var "b"),imp (neg $ Var "c") (neg $ Var "a")]++[neg $ imp (Var "b") (neg $ Var "c")]
ejer1_2 = error "Te toca"
ejer1_3 = error "Te toca"
ejer1_4 = error "Te toca"


ejer2_a = error "Te toca"
ejer2_b = error "Te toca"




