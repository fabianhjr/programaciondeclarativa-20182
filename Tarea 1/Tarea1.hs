{-
Facultad de Ciencias UNAM - Programación Declarativa 2018-2
Profesor: C. Moisés Vázquez Reyes
Ayudante: Enrique Antonio Bernal Cedillo
-}

module Tarea1 where

import Numeric.Natural(Natural)
import Data.List(intersect)
import Unificacion


{-----------------------}
-- CÁLCULO LAMBDA PURO --
{-----------------------}

data Lam_U = VarU Nombre | LamU Nombre Lam_U | AppU Lam_U Lam_U

instance Show Lam_U where
  show t = case t of
             VarU x     -> x
             LamU x e   -> "(λ" ++ x ++ "." ++ show e ++ ")"
             AppU e1 e2 -> "(" ++ show e1 ++ " " ++ show e2 ++ ")"


--
-- Variables Libres y Ligadas
--

-- | Checa si una variable aparece en una expresión
--
-- >>> "x" `esVarEn` (LamU "x" (VarU "y"))
-- False
-- >>> "x" `esVarEn` (VarU "x")
-- True
esVarEn :: Nombre -> Lam_U -> Bool
esVarEn b e = case e of
              (VarU v)     -> b == v
              (LamU _ e1)  -> b `esVarEn` e1
              (AppU e1 e2) -> b `esVarEn` e1 || b `esVarEn` e2

-- | Lista todas las variables libres en una expresión
--
-- >>> varsLibres (LamU "x" (VarU "x"))
-- []
-- >>> varsLibres (LamU "x" (VarU "y"))
-- ["y"]
varsLibres :: Lam_U -> [Nombre]
varsLibres e = case e of
                 (VarU v)     -> [v]
                 (LamU v e1)  -> varsLibres e1 `without` v
                 (AppU e1 e2) -> varsLibres e1 ++ varsLibres e2
  where l `without` x = filter (/= x) l


varsLigadasEn :: Lam_U -> Nombre -> [Nombre]
varsLigadasEn e b = case e of
                      (VarU _)     -> []
                      (LamU v e1)  -> if b `esVarEn` e1
                                      then v : varsLigadasEn e1 b
                                      else     varsLigadasEn e1 b
                      (AppU e1 e2) -> varsLigadasEn e1 b ++ varsLigadasEn e2 b

colisionanAlSustituir :: Nombre -> Lam_U -> Lam_U -> [Nombre]
colisionanAlSustituir b e s = varsLigadasEn e b `intersect` varsLibres s

--
-- Sustitución Inocente
--
sustituir :: Nombre -> Lam_U -> Lam_U -> Lam_U
sustituir b e s = case e of
                    (VarU v)     -> if v == b then s
                                    else VarU v
                    (LamU v e1)  -> if v == b then LamU v e1
                                    else LamU v $ sustituir b e1 s
                    (AppU e1 e2) -> AppU (sustituir b e1 s) (sustituir b e2 s)

--
-- Alpha Equivalencia de Variables Ligadas
--
alphaEquiv :: Lam_U -> Nombre -> Nombre -> Lam_U
alphaEquiv e b s
  | not . null $ colisionanAlSustituir b e (VarU s) = error "Error: Colision"
  | otherwise = case e of
                  (VarU v)     -> VarU v
                  (LamU v e1)  -> if v == b
                                  then LamU s $ sustituir b e1 (VarU s)
                                  else LamU v $ alphaEquiv e1 b s
                  (AppU e1 e2) -> AppU (alphaEquiv e1 b s) (alphaEquiv e2 b s)

alphaEquivAuto :: Lam_U -> Nombre -> Lam_U
alphaEquivAuto e b = alphaEquiv e b remplazo
  where
    posiblesRemplazos = tail $ iterate (++ "'") b
    pruebaRemplazo s' = null $ colisionanAlSustituir b e (VarU s')
    remplazo = head . filter pruebaRemplazo $ posiblesRemplazos

--
-- Checa si se puede β-reducir
--
betaRed :: Lam_U -> Bool
betaRed e = case e of
              (VarU _)            -> False
              (LamU _ e1)         -> betaRed e1
              (AppU (LamU _ _) _) -> True
              (AppU e1 e2)        -> betaRed e1 || betaRed e2

--
-- Aplica una β-reducción si es posible.
--
betaR :: Lam_U -> Lam_U
betaR (AppU (LamU v e) s)
  | null colisiones = sustituir v e s
  | otherwise = betaR (AppU (LamU v e') s)
  where
    colisiones = colisionanAlSustituir v e s
    e'         = foldl alphaEquivAuto e colisiones
betaR e = case e of
            (VarU v)     -> VarU v
            (LamU v e1)  -> LamU v $ betaR e1
            (AppU e1 e2) -> if betaRed e1 then AppU (betaR e1) e2
                            else AppU e1 (betaR e2)

--
-- Busca la formaNormal en orden normal
--
formaNormal :: Lam_U -> Lam_U
formaNormal (VarU x)    = VarU x
formaNormal (LamU x e1) = LamU x (formaNormal e1)
formaNormal (AppU (LamU n c) e2)      = formaNormal $ betaR $ AppU (LamU n c) e2
formaNormal (AppU e1 e2) | betaRed e1 = formaNormal $ AppU (betaR e1) e2
                         | betaRed e2 = AppU e1 (formaNormal e2)
                         | otherwise  = AppU e1 e2

--
-- Definiciones Generales en λ
--
lTrue  :: Lam_U
lTrue  = LamU "x" $ LamU "y" $ VarU "x"
lFalse :: Lam_U
lFalse = LamU "x" $ LamU "y" $ VarU "y"
lNot   :: Lam_U
lNot   = LamU "p" $ LamU "x" $ LamU "y" $ AppU (AppU (VarU "p") (VarU "y")) (VarU "x")
lPair  :: Lam_U
lPair  = LamU "x" $ LamU "y" $ LamU "p" $ AppU (AppU (VarU "p") (VarU "x")) (VarU "y")
lFst   :: Lam_U
lFst   = LamU "p" $ AppU (VarU "p") lTrue
lSnd   :: Lam_U
lSnd   = LamU "p" $ AppU (VarU "p") lFalse

--
-- Numerales de Church
--
lSucc :: Lam_U
lSucc = LamU "n" $ LamU "s" $ LamU "z" $ AppU (VarU "s") $ AppU (AppU (VarU "n") (VarU "s")) (VarU "z")
churchN :: Natural -> Lam_U
churchN 0 = LamU "s" $ LamU "z" $ VarU "z"
churchN n = formaNormal $ AppU lSucc (churchN (n-1))

--
-- Numerales de Scott
--

scottN :: Natural -> Lam_U
scottN 0 = LamU "x" $ LamU "y" (VarU "x")
scottN n = LamU "x" $ LamU "y" $ AppU (VarU "y") (scottN (n-1))

--
--Codifica los incisos de la pregunta 1
--
f1 :: Lam_U
f1 = LamU "n" $ LamU "m" $ LamU "s" $ LamU "z" $ AppU (AppU (AppU (VarU "m") (VarU "n")) (VarU "s")) (VarU "z")
g1 :: Lam_U
g1 = LamU "n" $ LamU "s" $ LamU "z" $ AppU (AppU (AppU (VarU "n") g1') (LamU "u" (VarU "z"))) (LamU "u" (VarU "u"))
  where g1' = LamU "h1" $ LamU "h2" $ AppU (VarU "h2") (AppU (VarU "h1") (VarU "s"))
h1 :: Lam_U
h1 = LamU "n" $ AppU lFst (AppU (AppU (VarU "n") ss) zz) -- λn.fst (n ss zz)
  where ss = LamU "p" $ AppU (AppU lPair (AppU lSnd (VarU "p"))) (AppU lSucc (AppU lSnd (VarU "p"))) -- λp.pair (snd p) (suc (snd p)),
        zz = AppU (AppU lPair (churchN 0)) (churchN 0) -- pair 0 0

--Cálculos
-- f1 5 0 = λs.λz.s z = 1
res1 :: Lam_U
res1 = formaNormal (AppU (AppU f1 (churchN 5)) (churchN 0))
-- f1 2 3 = λs.λz.s(s(s(s(s(s(s(s z))))))) = 8
res2 :: Lam_U
res2 = formaNormal (AppU (AppU f1 (churchN 2)) (churchN 3))

-- g1 0 = λs.λz.z = 0
res3 :: Lam_U
res3 = formaNormal (AppU g1 (churchN 0))
-- g1 3 = λs.λz.s(s z)) = 2
res4 :: Lam_U
res4 = formaNormal (AppU g1 (churchN 3))

-- h1 1 = λs.λz.z = 0
res5 :: Lam_U
res5 = formaNormal (AppU h1 (churchN 1))
-- h1 2 = λs.λz'.s z' = 1
res6 :: Lam_U
res6 = formaNormal (AppU h1 (churchN 2))

--Codifica los incisos de la pregunta 2
f2 :: Lam_U
f2 = LamU "n" $ LamU "x" $ LamU "y" $ AppU (VarU "y") (VarU "n") -- λn.λx.λy.yn
g2 :: Lam_U
g2 = LamU "n" $ AppU (AppU (VarU "n") (scottN 0)) (LamU "x" (VarU "x")) -- λn.n 0 (λx.x)
h2 :: Lam_U
h2 = LamU "n" $ AppU (AppU (VarU "n") lTrue) (LamU "x" lFalse) -- λn.n true (λx.false)

-- Cálculos
-- f2 0 = λx.λy.y(λx.λy.x) = 1
res7 :: Lam_U
res7 = formaNormal (AppU f2 (scottN 0))
-- f2 3 = λx.λy.y(λx.λy.y(λx.λy.y(λx.λy.y(λx.λy.x)))) = 4
res8 :: Lam_U
res8 = formaNormal (AppU f2 (scottN 3))

-- g2 1 = λx.λy.x = 0
res9 :: Lam_U
res9 = formaNormal (AppU g2 (scottN 1))
-- g2 4 = λx.λy.y(λx.λy.y(λx.λy.y(λx.λy.x))) = 3
res10 :: Lam_U
res10 = formaNormal (AppU g2 (scottN 4))

-- h2 0 = λx.λy.x = True
res11 :: Lam_U
res11 = formaNormal (AppU h2 (scottN 0))
-- h2 5 = λx.λy.y = False
res12 :: Lam_U
res12 = formaNormal (AppU h2 (scottN 5))

--Codifica los incisos de la pregunta 3
yCombinator :: Lam_U
yCombinator = LamU "f" $ AppU (LamU "x" (AppU (VarU "f") (AppU (VarU "x") (VarU "x")))) (LamU "x" (AppU (VarU "f") (AppU (VarU "x") (VarU "x"))))

-- TODO: Pendiente a resolver g2
casiImpar :: Lam_U
casiImpar = LamU "f" $ LamU "n" $ AppU (AppU (AppU h2 (VarU "n")) lFalse) (AppU lNot (AppU (VarU "f") (AppU g2 (VarU "n"))))
impar :: Lam_U
impar = AppU yCombinator casiImpar

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
w :: LamAB -> [Nombre] -> (Juicio,[Nombre])
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
