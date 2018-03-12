{-
Facultad de Ciencias UNAM - Programación Declarativa 2018-2
Profesor: C. Moisés Vázquez Reyes
Ayudante: Enrique Antonio Bernal Cedillo
-}

import Numeric.Natural(Natural)
import Data.Maybe(fromMaybe)
import Data.List(intersect)
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

-- Variables Libres y Ligadas
variablesLigadas :: Lam_U -> [Nombre]
variablesLigadas (VarU _) = []
variablesLigadas (LamU v e)   = v : variablesLigadas e
variablesLigadas (AppU e1 e2) = variablesLigadas e1 ++ variablesLigadas e2

variablesLibres :: Lam_U -> [Nombre]
variablesLibres (VarU v) = [v]
variablesLibres (LamU v e) = (variablesLibres e) `sin` v
  where xs `sin` x = filter (/= x) xs
variablesLibres (AppU e1 e2) = variablesLibres e1 ++ variablesLibres e2

--Sustitución
sust :: Lam_U -> Nombre -> Lam_U -> Lam_U
sust (VarU v)     o s = if (v == o) then s else (VarU v)
sust (LamU v e)   o s = if (v == o) then LamU v e else LamU v (sust e o s)
sust (AppU e1 e2) o s = AppU (sust e1 o s) (sust e2 o s)

--Gamma Equivalencia de Variables Ligadas
gammaEquiv :: Lam_U -> (Nombre, Nombre) -> Lam_U
gammaEquiv (VarU v)     (_,_) = (VarU v)
gammaEquiv (LamU v e)   (o,s) = if (v == o) then (LamU s (sust e o (VarU s)))
                                            else (LamU v (gammaEquiv e (o,s)))
gammaEquiv (AppU e1 e2) (o,s) = (AppU (gammaEquiv e1 (o,s)) (gammaEquiv e2 (o,s)))

--Aplica la β-reducción de dos términos
betaR :: Lam_U -> Lam_U
betaR (AppU (LamU v c) e)
  | null varsCol = sust c v e
  | otherwise    = betaR (AppU (LamU v' c') e)
    where
      varsCol  = (variablesLigadas c) `intersect` (variablesLibres e)
      varsCol' = map (++"'") varsCol
      remplazo = zip varsCol varsCol'
      c' = foldl (gammaEquiv) c remplazo
      v' = fromMaybe v (lookup v remplazo)
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
-- Definiciones generales
true'  = LamU "x" $ LamU "y" $ VarU "x"
false' = LamU "x" $ LamU "y" $ VarU "y"
pair   = LamU "x" $ LamU "y" $ LamU "p" $ AppU (AppU (VarU "p") (VarU "x")) (VarU "y")
fst'   = LamU "p" $ AppU (VarU "p") true'
snd'   = LamU "p" $ AppU (VarU "p") false'

succ' = LamU "n" $ LamU "s" $ LamU "z" $ AppU (VarU "s") $ AppU (AppU (VarU "n") (VarU "s")) (VarU "z")
churchN :: Natural -> Lam_U
churchN 0 = LamU "s" $ LamU "z" $ VarU "z"
churchN n = formaNormal (AppU succ' (churchN (n-1)))


f1 = LamU "n" $ LamU "m" $ LamU "s" $ LamU "z" $ AppU (AppU (AppU (VarU "m") (VarU "n")) (VarU "s")) (VarU "z")
g1 = LamU "n" $ LamU "s" $ LamU "z" $ AppU (AppU (AppU (VarU "n") g1') (LamU "u" (VarU "z"))) (LamU "u" (VarU "u"))
  where g1' = LamU "h1" $ LamU "h2" $ AppU (VarU "h2") (AppU (VarU "h1") (VarU "s"))
h1 = LamU "n" $ AppU fst' (AppU (AppU (VarU "n") ss) zz)
  where ss = LamU "p" (AppU (AppU pair (AppU snd' (VarU "p"))) (AppU succ' (AppU snd' (VarU "p"))))
        zz = AppU (AppU pair (churchN 0)) (churchN 0)

--Cálculos
-- f1 5 0 = λs.λz.s z = 1
res1 = formaNormal (AppU (AppU f1 (churchN 5)) (churchN 0))
-- f1 2 3 = λs.λz.s(s(s(s(s(s(s(s z))))))) = 8
res2 = formaNormal (AppU (AppU f1 (churchN 2)) (churchN 3))

-- g1 0 = λs.λz.z = 0
res3 = formaNormal (AppU g1 (churchN 0))
-- g1 3 = λs.λz.s(s z)) = 2
res4 = formaNormal (AppU g1 (churchN 3))

-- h1 1 = λs.λz.z = 0
res5 = formaNormal (AppU h1 (churchN 1))
-- h1 2 = λs.λz'.s z' = 1
res6 = formaNormal (AppU h1 (churchN 2))

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
