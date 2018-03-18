{-
Facultad de Ciencias UNAM - Programación Declarativa 2018-2
Profesor: C. Moisés Vázquez Reyes
Ayudante: Enrique Antonio Bernal Cedillo
-}

module Tarea1 where

import Data.Maybe (fromMaybe)
import Numeric.Natural (Natural)
import Data.Char (isNumber)
import Data.List (intersect)
import Unificacion

{-----------------------}
-- CÁLCULO LAMBDA PURO --
{-----------------------}

data Lam_U =
       VarU Nombre
     | LamU Nombre Lam_U
     | AppU Lam_U Lam_U

instance Show Lam_U where
  show t = case t of
             VarU x     -> x
             LamU x e   -> "(λ" ++ x ++ "." ++ show e ++ ")"
             AppU e1 e2 -> "(" ++ show e1 ++ " " ++ show e2 ++ ")"

--
-- Variables Libres y Ligadas
--

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


-- | Lista todas las variables ligadas en una expresión para una variable libre dada
--
varsLigadasEn :: Lam_U -> Nombre -> [Nombre]
varsLigadasEn e b = case e of
                      (VarU _)     -> []
                      (LamU v e1)  -> if b /= v && b `elem` varsLibres e1
                                      then v : varsLigadasEn e1 b else []
                      (AppU e1 e2) -> varsLigadasEn e1 b ++ varsLigadasEn e2 b

-- | Obtiene una lista de variables libres que se ligan al sustituir
--
colisionanAlSustituir :: Nombre -> Lam_U -> Lam_U -> [Nombre]
colisionanAlSustituir b e s = varsLigadasEn e b `intersect` varsLibres s

--
-- Operaciones de LamU
--

-- | Sustitución Inocente de Variables Libres
--
sustituir :: Nombre -> Lam_U -> Lam_U -> Lam_U
sustituir b e s = case e of
                    (VarU v)     -> if v == b then s
                                    else VarU v
                    (LamU v e1)  -> if v == b then LamU v e1
                                    else LamU v $ sustituir b e1 s
                    (AppU e1 e2) -> AppU (sustituir b e1 s) (sustituir b e2 s)

-- | α-equivalencia de Variables Ligadas
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

-- | Busca la primer α-equivalencia de añádir tildes sin colisionar con variables libres
--
alphaEquivAuto :: Lam_U -> Nombre -> Lam_U
alphaEquivAuto e b = alphaEquiv e b remplazo
  where
    posiblesRemplazos = tail $ iterate (++ "'") b
    pruebaRemplazo s' = null $ colisionanAlSustituir b e (VarU s')
    remplazo = head . filter pruebaRemplazo $ posiblesRemplazos

-- | Checa si se puede β-reducir
--
betaRed :: Lam_U -> Bool
betaRed e = case e of
              (VarU _)            -> False
              (LamU _ e1)         -> betaRed e1
              (AppU (LamU _ _) _) -> True
              (AppU e1 e2)        -> betaRed e1 || betaRed e2

-- | Aplica la primer β-reducción posible.
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

-- | Busca la formaNormal en orden normal
--
formaNormal :: Lam_U -> Lam_U
formaNormal e = case e of
                  (VarU x)             -> VarU x
                  (LamU x e1)          -> LamU x (formaNormal e1)
                  (AppU (LamU n c) e2) -> formaNormal $ betaR $ AppU (LamU n c) e2
                  (AppU e1 e2)         -> if betaRed e1
                                          then formaNormal $ AppU (betaR e1) e2
                                          else               AppU e1 (formaNormal e2)

--
-- Definiciones Generales en λ
--
lTrue  :: Lam_U
lTrue  = LamU "x" $ LamU "y" $ VarU "x"
lFalse :: Lam_U
lFalse = LamU "x" $ LamU "y" $ VarU "y"
lNot   :: Lam_U
lNot   = LamU "p" $ LamU "x" $ LamU "y" $ AppU (AppU (VarU "p") (VarU "y")) (VarU "x")

lConst :: Lam_U
lConst = LamU "c" $ LamU "x" $ VarU "c"

lPair  :: Lam_U
lPair  = LamU "x" $ LamU "y" $ LamU "p" $ AppU (AppU (VarU "p") (VarU "x")) (VarU "y")
lFst   :: Lam_U
lFst   = LamU "p" $ AppU (VarU "p") lTrue
lSnd   :: Lam_U
lSnd   = LamU "p" $ AppU (VarU "p") lFalse

--
-- Numerales de Church
--
churchSucc :: Lam_U
churchSucc = LamU "n" $ LamU "s" $ LamU "z" $
               AppU (VarU "s") $ AppU (AppU (VarU "n") (VarU "s")) (VarU "z")
churchN :: Natural -> Lam_U
churchN 0 = LamU "s" $ LamU "z" $ VarU "z"
churchN n = formaNormal $ AppU churchSucc (churchN (n-1))

-- | Checa si un numeral de church es cero
--
-- >>> formaNormal (AppU churchEsCero (churchN 0))
-- (λx.(λy.x))
-- >>> formaNormal (AppU churchEsCero (churchN 9))
-- (λx.(λy.y))
churchEsCero :: Lam_U
churchEsCero = LamU "n" $ AppU (AppU (VarU "n") (AppU lConst lFalse)) lTrue

-- | Suma dos numerales de church
--
-- >>> formaNormal (AppU (AppU churchSum (churchN 2)) (churchN 3))
-- (λs.(λz.(s (s (s (s (s z)))))))
-- >>> formaNormal (AppU (AppU churchSum (churchN 5)) (churchN 10))
-- (λs.(λz.(s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))
churchSum :: Lam_U
churchSum = LamU "n" $ LamU "m" $ LamU "s" $ LamU "z" $
              AppU (AppU (VarU "n") (VarU "s")) (AppU (AppU (VarU "m") (VarU "s")) (VarU "z"))

-- | Multiplica dos numerales de church
--
-- >>> formaNormal (AppU (AppU churchProd (churchN 2)) (churchN 4))
-- (λs.(λz.(s (s (s (s (s (s (s (s z))))))))))
-- >>> formaNormal (AppU (AppU churchProd (churchN 3)) (churchN 5))
-- (λs.(λz.(s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))
churchProd :: Lam_U
churchProd = LamU "n" $ LamU "m" $ LamU "s" $ AppU (VarU "n") (AppU (VarU "m") (VarU "s"))

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
  where ss = LamU "p" $ AppU (AppU lPair (AppU lSnd (VarU "p"))) (AppU churchSucc (AppU lSnd (VarU "p"))) -- λp.pair (snd p) (suc (snd p)),
        zz = AppU (AppU lPair (churchN 0)) (churchN 0) -- pair 0 0

--Cálculos

-- |
-- >>> res1
-- (λs.(λz.(s z)))
res1 :: Lam_U
res1 = formaNormal (AppU (AppU f1 (churchN 5)) (churchN 0))

-- |
-- >>> res2
-- (λs.(λz.(s (s (s (s (s (s (s (s z))))))))))
res2 :: Lam_U
res2 = formaNormal (AppU (AppU f1 (churchN 2)) (churchN 3))

-- |
-- >>> res3
-- (λs.(λz.z))
res3 :: Lam_U
res3 = formaNormal (AppU g1 (churchN 0))
-- |
-- >>> res4
-- (λs.(λz.(s (s z))))
res4 :: Lam_U
res4 = formaNormal (AppU g1 (churchN 3))

-- |
-- >>> res5
-- (λs.(λz.z))
res5 :: Lam_U
res5 = formaNormal (AppU h1 (churchN 1))
-- |
-- >>> res6
-- (λs.(λz.(s z)))
res6 :: Lam_U
res6 = formaNormal (AppU h1 (churchN 2))

--
-- Codifica los incisos de la pregunta 2
--
f2 :: Lam_U
f2 = LamU "n" $ LamU "x" $ LamU "y" $ AppU (VarU "y") (VarU "n") -- λn.λx.λy.yn
g2 :: Lam_U
g2 = LamU "n" $ AppU (AppU (VarU "n") (scottN 0)) (LamU "x" (VarU "x")) -- λn.n 0 (λx.x)
h2 :: Lam_U
h2 = LamU "n" $ AppU (AppU (VarU "n") lTrue) (LamU "x" lFalse) -- λn.n true (λx.false)

-- Cálculos

-- |
-- >>> res7
-- (λx.(λy.(y (λx.(λy.x)))))
res7 :: Lam_U
res7 = formaNormal (AppU f2 (scottN 0))
-- |
-- >>> res8
-- (λx.(λy.(y (λx.(λy.(y (λx.(λy.(y (λx.(λy.(y (λx.(λy.x))))))))))))))
res8 :: Lam_U
res8 = formaNormal (AppU f2 (scottN 3))

-- |
-- >>> res9
-- (λx.(λy.x))
res9 :: Lam_U
res9 = formaNormal (AppU g2 (scottN 1))
-- |
-- >>> res10
-- (λx.(λy.(y (λx.(λy.(y (λx.(λy.(y (λx.(λy.x)))))))))))
res10 :: Lam_U
res10 = formaNormal (AppU g2 (scottN 4))

-- |
-- >>> res11
-- (λx.(λy.x))
res11 :: Lam_U
res11 = formaNormal (AppU h2 (scottN 0))
-- |
-- >>> res12
-- (λx.(λy.y))
res12 :: Lam_U
res12 = formaNormal (AppU h2 (scottN 5))

--
-- Codifica los incisos de la pregunta 3
--
-- | El combinador-y de punto fijo.
--   Advertencia: Evitar buscar la forma normal sin hacer una aplicación completa.
yCombinator :: Lam_U
yCombinator = LamU "f" $ AppU (LamU "x" (AppU (VarU "f") (AppU (VarU "x") (VarU "x")))) (LamU "x" (AppU (VarU "f") (AppU (VarU "x") (VarU "x"))))

-- | Determina el caso base de exponencias o reduce un caso que se le pase.
--   Para uso con el combinador-Y, recibe una pareja de numerales de Church.
casiChurchExponente :: Lam_U
casiChurchExponente = LamU "f" $ LamU "nm" $
                        AppU (AppU (AppU churchEsCero m)
                          (churchN 1))
                          (AppU (AppU churchProd n) (AppU (VarU "f") nm'))
  where n = AppU lFst (VarU "nm")
        m = AppU lSnd (VarU "nm")
        nm' = AppU (AppU lPair n) $ AppU g1 m

-- | Exponencia una pareja de numerales de Church
--
-- >>> formaNormal $ AppU churchExponente $ AppU (AppU lPair (churchN 1)) (churchN 5)
-- (λs.(λz.(s z)))
-- >>> formaNormal $ AppU churchExponente $ AppU (AppU lPair (churchN 3)) (churchN 2)
-- (λs.(λz.(s (s (s (s (s (s (s (s (s z)))))))))))
churchExponente :: Lam_U
churchExponente = AppU yCombinator casiChurchExponente

-- | Determina el caso base de scottImpar o reduce un caso que se le pase.
--   Para uso con el combinador-Y, recibe numerales de Scott.
casiScottImpar :: Lam_U
casiScottImpar = LamU "f" $ LamU "n" $
                   AppU (AppU (AppU scottEsCero (VarU "n"))
                     lFalse)
                     (AppU lNot (AppU (VarU "f") (AppU scottPred (VarU "n"))))
  where scottEsCero = h2
        scottPred = g2

-- | Determina si un numeral de Scott es scottImpar
--
-- >>> formaNormal $ AppU scottImpar $ scottN 0
-- (λx.(λy.y))
-- >>> formaNormal $ AppU scottImpar $ scottN 3
-- (λx.(λy.x))
-- >>> formaNormal $ AppU scottImpar $ scottN 4
-- (λx.(λy.y))
-- >>> formaNormal $ AppU scottImpar $ scottN 7
-- (λx.(λy.x))
scottImpar :: Lam_U
scottImpar = AppU yCombinator casiScottImpar






{-----------------------}
-- INFERENCIA DE TIPOS --
{-----------------------}

--
-- Expresiones LamAB sin anotaciones de tipos.
--

data LamAB =
       VNum Int
     | VBool Bool
     | Var Nombre
     | Suma LamAB LamAB
     | Prod LamAB LamAB
     | Ifte LamAB LamAB LamAB
     | Iszero LamAB
     | Lam Nombre LamAB
     | App LamAB LamAB
     deriving Show

--
-- Expresiones LamAB con anotaciones de tipos.
--
data LamABT =
       VNumT Int
     | VBoolT Bool
     | VarT Nombre
     | SumaT LamABT LamABT
     | ProdT LamABT LamABT
     | IfteT LamABT LamABT LamABT
     | IszeroT LamABT
     | LamT Nombre Tipo LamABT
     | AppT LamABT LamABT
     deriving Show

--
-- Para representar un contexto de variables [(x1,T1),...,(xn,Tn)].
--

type Ctx = [(Nombre,Tipo)]

--
-- Para representar juicios de tipado.
--

data Juicio = Deriv (Ctx,LamABT,Tipo)

instance Show Juicio where
  show (Deriv (ctx, e, t)) = show ctx++" ⊢ "++show e++" : "++show t

--
-- Algoritmo W
--
-- | Realiza la inferencia de tipos de una expresión LamABT
--
-- >>> algoritmoW $ VNum 10
-- [] ⊢ VNumT 10 : ℕ
-- >>> algoritmoW $ VBool False
-- [] ⊢ VBoolT False : 𝔹
--
-- >>> algoritmoW $ Lam "x" (Var "x")
-- [] ⊢ LamT "x" X0 (VarT "x") : (X0->X0)
-- >>> algoritmoW $ Lam "x" (Var "y")
-- [("y",X0)] ⊢ LamT "x" X1 (VarT "y") : (X1->X0)
-- >>> algoritmoW $ Lam "x" $ Lam "y" $ Var "y"
-- [] ⊢ LamT "x" X1 (LamT "y" X0 (VarT "y")) : (X1->(X0->X0))
--
-- >>> algoritmoW $ App (Lam "x" (Var "y")) (VBool True)
-- [("y",X0)] ⊢ AppT (LamT "x" X1 (VarT "y")) (VBoolT True) : X0
-- >>> algoritmoW $ App (Lam "x" (Var "x")) (VBool True)
-- [] ⊢ AppT (LamT "x" X0 (VarT "x")) (VBoolT True) : 𝔹
-- >>> algoritmoW $ App (Lam "x" (Var "x")) (Var "y")
-- [("y",X1)] ⊢ AppT (LamT "x" X0 (VarT "x")) (VarT "y") : X1
--
-- >>> algoritmoW $ Lam "s" $ Lam "z" $ App (Var "s") (Var "z")
-- [] ⊢ LamT "s" (X1->X2) (LamT "z" X1 (AppT (VarT "s") (VarT "z"))) : ((X1->X2)->(X1->X2))
--
-- >!> algoritmoW $ App (App (Var "x") (Var "y")) (Var "z")
-- [("x",(X1->(X2->X3))),("y",X1),("z",X2)] ⊢ AppT (AppT (VarT "x") (VarT "y")) (VarT "z") : X3
--
-- >!> algoritmoW $ App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))
-- [("x",X6->(X4->X0)),("z",X6),("y",X6->X4),("z",X6)]|-AppT (AppT (VarT "x") (VarT "z")) (AppT (VarT "y") (VarT "z")):X0
--
-- >>> algoritmoW $ Lam "f" $ Lam "x" $ Lam "y" $ App (Var "f") (Suma (Var "x") (Var "y"))
-- [] ⊢ LamT "f" (ℕ->X3) (LamT "x" ℕ (LamT "y" ℕ (AppT (VarT "f") (SumaT (VarT "x") (VarT "y"))))) : ((ℕ->X3)->(ℕ->(ℕ->X3)))
--
-- >!> algoritmoW $ App (Var "g") (App (Var "f") (Prod (VNum 3) (Var "z")))
-- [("g",X2->X0),("f",Nat->X2),("z",Nat)]|-AppT (VarT "g") (AppT (VarT "f") (ProdT (VNumT 3) (VarT "z"))):X0
--
-- >>> algoritmoW $ Ifte (Iszero $ Suma (VNum 2) (VNum 0)) (App (Var "f") (Var "y")) (VBool False)
-- [("f",(X1->𝔹)),("y",X1)] ⊢ IfteT (IszeroT (SumaT (VNumT 2) (VNumT 0))) (AppT (VarT "f") (VarT "y")) (VBoolT False) : 𝔹
-- >>> algoritmoW $ Lam "x" $ Lam "y" $ Ifte (VBool True) (App (Var "f") (Var "x")) (Var "y")
-- [("f",(X1->X2))] ⊢ LamT "x" X1 (LamT "y" X2 (IfteT (VBoolT True) (AppT (VarT "f") (VarT "x")) (VarT "y"))) : (X1->(X2->X2))
--
-- >!> algoritmoW $ App (Var "x") (Var "x")
-- *** Exception: No se pudo unificar.
-- >>> algoritmoW $ App (Suma (VNum 1) (Var "n")) (Var "w")
-- *** Exception: Error de Tipado.
-- ...
algoritmoW :: LamAB->Juicio
algoritmoW e = Deriv (elimRep ctx, e', t)
  where
    (Deriv (ctx, e', t), _) = w e []
    elimRep [] = []
    elimRep (x:xs) = x : filter (x/=) (elimRep xs)

w :: LamAB -> [Nombre] -> (Juicio, [Nombre])
w (Lam e1 e2) vars = (Deriv (ctx'', LamT e1 t1'' e2', t1'' :-> t2''), vars')
  where
    (Deriv (ctx2', e2', t2'), vars2') = w e2 vars
    ctx'  = unificaSust $ filter (\(n, _) -> n /= e1) ctx2'
    t1'   = fromMaybe (X $ head $ sigLib vars2') $ lookup e1 ctx2'
    t1''  = apSustT t1' ctx'
    t2''  = apSustT t2' ctx'
    ctx'' = quitarRedundates ctx'
    vars' = vars2'


w (App e1 e2) vars = (Deriv (ctx'', AppT e1' e2', t''), vars')
  where
    (Deriv (ctx1', e1', t1'), vars1') = w e1 vars
    (Deriv (ctx2', e2', t2'), vars2') = w e2 vars1'
    ctx' = unificaSust $
      case t1' of
        X n     -> foldl compSust ctx1' $ ctx2' : [(n, X n :-> (X $ head $ sigLib vars2'))] : unifica (X n) t2'
        e :-> _ -> foldl compSust ctx1' $ ctx2' : unifica e t2'
        _       -> error "Error de Tipado."
    t'  = apSustT t1' ctx'
    (ctx'', t'') = case t' of
                     _ :-> s -> (quitarRedundates ctx', s)
                     _       -> error "Error de Tipado/Unificacion."
    vars' = vars2'


w (Suma e1 e2) vars = (Deriv (ctx', SumaT e1' e2', TNat), vars')
  where
    (Deriv (ctx1', e1', t1'), vars1') = w e1 vars
    (Deriv (ctx2', e2', t2'), vars2') = w e2 vars1'
    ctx'  = quitarRedundates $ unificaSust $
      foldl compSust ctx1' $ ctx2' : (unifica t1' TNat ++ unifica t2' TNat)
    vars' = vars2'
w (Prod e1 e2) vars = (Deriv (ctx', ProdT e1' e2', TNat), vars')
  where
    (Deriv (ctx1', e1', t1'), vars1') = w e1 vars
    (Deriv (ctx2', e2', t2'), vars2') = w e2 vars1'
    ctx'  = quitarRedundates $ unificaSust $
      foldl compSust ctx1' $ ctx2' : (unifica t1' TNat ++ unifica t2' TNat)
    vars' = vars2'


w (Ifte e1 e2 e3) vars = (Deriv (ctx'', IfteT e1' e2' e3', t'), vars')
  where
    (Deriv (ctx1', e1', t1'), vars1') = w e1 vars
    (Deriv (ctx2', e2', t2'), vars2') = w e2 vars1'
    (Deriv (ctx3', e3', t3'), vars3') = w e3 vars2'
    ctx'  = unificaSust $
      foldl compSust ctx1' $ ctx2' : ctx3' : (unifica t1' TBool ++ unifica t2' t3')
    t'    = apSustT t2' ctx'
    ctx'' = quitarRedundates ctx'
    vars' = vars3'
w (Iszero e1) vars = (Deriv (ctx', IszeroT e1', TBool), vars')
  where
    (Deriv (ctx1', e1', t1'), vars1') = w e1 vars
    ctx'  = quitarRedundates $ unificaSust $
      foldl compSust ctx1' $ unifica t1' TNat
    vars' = vars1'


w e vars = case e of
             (VNum n)  -> (Deriv ([], VNumT n,  TNat),  vars)
             (VBool b) -> (Deriv ([], VBoolT b, TBool), vars)
             (Var n)   -> (Deriv ([(n, X t)], VarT  n, X t), t:vars)
               where
                 t = head $ sigLib vars
             _         -> undefined



sigLib :: [Nombre] -> [Nombre]
sigLib vars = filter (`notElem` vars) $ map (\n -> "X" ++ show n) [0..]

quitarRedundates :: Ctx -> Ctx
quitarRedundates = filter (\(n,_) -> not $ (head n == 'X') && all isNumber (tail n))
