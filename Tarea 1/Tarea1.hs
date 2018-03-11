{-
Facultad de Ciencias UNAM - Programación Declarativa 2018-2
Profesor: C. Moisés Vázquez Reyes
Ayudante: Enrique Antonio Bernal Cedillo
-}

import Unificacion

{-----------------------}
-- CÁLCULO LAMBDA PURO --
{-----------------------}

data Lam_U = VarU Nombre | LamU Nombre Lam_U | AppU Lam_U Lam_U

instance Show Lam_U where
  show t = case t of
             VarU x   -> x
             LamU x e -> "λ" ++ x ++ "." ++ show e
             AppU (VarU x) (VarU y) -> x ++ " " ++ y
             AppU (VarU x) e2       -> x ++ "(" ++ show e2 ++ ")"
             AppU e1 (VarU y)       -> "(" ++ show e1 ++ ")" ++ y
             AppU e1 e2             -> "(" ++ show e1 ++ ")" ++ " " ++ "(" ++ show e2 ++ ")"

--Sustitución
sust :: Lam_U -> Nombre -> Lam_U -> Lam_U
sust (VarU var)        nombre e = if (var == nombre) then e else (VarU var)
sust (LamU var cuerpo) nombre e = LamU var (sust cuerpo nombre e)
sust (AppU l1 l2)      nombre e = AppU (sust l1 nombre e) (sust l2 nombre e)

--Aplica la β-reducción de dos términos
betaR :: Lam_U -> Lam_U
betaR (AppU (LamU nombre cuerpo) e) = sust cuerpo nombre e
betaR l = l

-- Checa si se puede β-reducir
betaRed :: Lam_U -> Bool
betaRed (VarU _)   = False
betaRed (LamU _ e) = betaRed e
betaRed (AppU (LamU _ _) _) = True
betaRed (AppU e1 e2) = betaRed e1 || betaRed e2

formaNormal::Lam_U->Lam_U
formaNormal (VarU x)     = VarU x
formaNormal (LamU x e)   = LamU x (formaNormal e)
formaNormal (AppU (LamU n c) e2) = formaNormal . betaR $ (AppU (LamU n c) e2)
formaNormal (AppU e1 e2) | betaRed (AppU e1 e2) = formaNormal (AppU (formaNormal e1) (formaNormal e2))
                         | otherwise = (AppU e1 e2)

--Codifica los incisos de la pregunta 1

true'  = LamU "x" $ LamU "y" $ VarU "x"
false' = LamU "x" $ LamU "y" $ VarU "y"
pair   = LamU "x" $ LamU "y" $ LamU "p" $ AppU (AppU (VarU "p") (VarU "x")) (VarU "y")
fst'   = LamU "p" $ AppU (VarU "p") true'
snd'   = LamU "p" $ AppU (VarU "p") false'

f1 = LamU "n" $ LamU "m" $ LamU "s" $ LamU "z" $ AppU (AppU (AppU (VarU "m") (VarU "n")) (VarU "s")) (VarU "z")
g1 = LamU "n" $ LamU "s" $ LamU "z" $ AppU (VarU "n") $ AppU (AppU g1' (LamU "u" (VarU "z"))) (LamU "u" (VarU "u"))
  where g1' = LamU "h1" $ LamU "h2" $ AppU (VarU "h2") (AppU (VarU "h1") (VarU "s"))

--Codifica los incisos de la pregunta 2

--Codifica los incisos de la pregunta 3

{-----------------------}
--INFERENCIA DE TIPOS--
{-----------------------}
--Expresiones LamAB sin anotaciones de tipos.
data LamAB = VNum Int
     | VBool Bool
     | Var Nombre
     | Suma LamAB LamAB
     | Prod LamAB LamAB
     | Ifte LamAB LamAB LamAB
     | Iszero LamAB
     | Lam Nombre LamAB
     | App LamAB LamAB
     deriving Show


--Expresiones LamAB con anotaciones de tipos.
data LamABT = VNumT Int
     | VBoolT Bool
     | VarT Nombre
     | SumaT LamABT LamABT
     | ProdT LamABT LamABT
     | IfteT LamABT LamABT LamABT
     | IszeroT LamABT
     | LamT Nombre Tipo LamABT
     | AppT LamABT LamABT
     deriving Show


--Para representar un contexto de variables [(x1,T1),...,(xn,Tn)].
type Ctx = [(Nombre,Tipo)]

--Para representar juicios de tipado.
data Juicio = Deriv (Ctx,LamABT,Tipo)

instance Show Juicio where
    show (Deriv (ctx, e, t)) = show ctx++" ⊢ "++show e++" : "++show t




--Realiza la inferencia de tipos de una expresión LamABT
algoritmoW :: LamAB->Juicio
algoritmoW e = let (Deriv (ctx,e',t),_) = w e [] in Deriv (elimRep ctx,e',t) where
                                                           elimRep [] = []
                                                           elimRep (x:xs) = x:(filter (x/=) $ elimRep xs)


--Realiza el algoritmo W en una expresión LamAB utilizando una lista de nombres que ya están ocupados.
w::LamAB->[Nombre]->(Juicio,[Nombre])
w e vars = error "Te toca"


{-PRUEBAS:-}
-- []|-LamT "x" X1 (LamT "y" X0 (VarT "y")):X1->(X0->X0)
prueba1 = algoritmoW $ Lam "x" $ Lam "y" $ Var "y"

-- [("x",X3->(X4->X0)),("y",X3),("z",X4)]|-AppT (AppT (VarT "x") (VarT "y")) (VarT "z"):X0
prueba2 = algoritmoW $ App (App (Var "x") (Var "y")) (Var "z")

-- *** Exception: No se pudo unificar.
prueba3 = algoritmoW $ App (Var "x") (Var "x")

-- []|-LamT "s" X2->X0 (LamT "z" X2 (AppT (VarT "s") (VarT "z"))):(X2->X0)->(X2->X0)
prueba4 = algoritmoW $ Lam "s" $ Lam "z" $ App (Var "s") (Var "z")

-- [("x",X6->(X4->X0)),("z",X6),("y",X6->X4),("z",X6)]|-AppT (AppT (VarT "x") (VarT "z")) (AppT (VarT "y") (VarT "z")):X0
prueba5 = algoritmoW $ App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))

-- []|-LamT "f" Nat->X0 (LamT "x" Nat (LamT "y" Nat (AppT (VarT "f") (SumaT (VarT "x") (VarT "y"))))):(Nat->X0)->(Nat->Nat->X0)
prueba6 = algoritmoW $ Lam "f" $ Lam "x" $ Lam "y" $ App (Var "f") (Suma (Var "x") (Var "y")) 

-- [("g",X2->X0),("f",Nat->X2),("z",Nat)]|-AppT (VarT "g") (AppT (VarT "f") (ProdT (VNumT 3) (VarT "z"))):X0
prueba7 = algoritmoW $ App (Var "g") (App (Var "f") (Prod (VNum 3) (Var "z")))

-- [("f",X2->Bool),("y",X2)]|-IfteT (IszeroT (SumaT (VNumT 2) (VNumT 0))) (AppT (VarT "f") (VarT "y")) (VBoolT False):Bool
prueba8 = algoritmoW $ Ifte (Iszero $ Suma (VNum 2) (VNum 0)) (App (Var "f") (Var "y")) (VBool False)

-- [("f",X2->X3)]|-LamT "x" X2 (LamT "y" X3 (IfteT (VBoolT True) (AppT (VarT "f") (VarT "x")) (VarT "y"))):X2->(X3->X3)
prueba9 = algoritmoW $ Lam "x" $ Lam "y" $ Ifte (VBool True) (App (Var "f") (Var "x")) (Var "y")

-- *** Exception: No se pudo unificar.
prueba10 = algoritmoW $ App (Suma (VNum 1) (Var "n")) (Var "w")
