{-Facultad de Ciencias UNAM - Programación Declarativa 2018-2 
      Profesor: C. Moisés Vázquez Reyes
      Ayudante: Enrique Antonio Bernal Cedillo
-}

module Unificacion where

import Data.Maybe (fromMaybe)

infixr :-> {- Así, el poderador ':->' asocia a la derecha. -}
type Nombre = String

-- Categoría de tipos.
data Tipo = TNat | TBool | X Nombre | Tipo :-> Tipo deriving Eq


instance Show Tipo where
     show t = case t of
            TNat      -> "ℕ"
            TBool     -> "𝔹"
            X name    -> name
            t1 :-> t2 -> "(" ++ show t1 ++ "->" ++ show t2 ++")"

--Una sustitución es un conjunto de la forma [(xi, Ti)]
type Sust = [(Nombre, Tipo)]

--Elimina sustituciones de la forma [X:=X] en una sustitución.
simpSust::Sust->Sust
simpSust = filter checar
  where checar (n, t) = case t of
                          (X n') -> n /= n'
                          _      -> True

--Aplica una sustitución a un tipo.
apSustT::Tipo->Sust->Tipo
apSustT t sust = case t of
                   TNat  -> TNat
                   TBool -> TBool
                   (X n)     -> fromMaybe (X n) $ lookup n sust
                   t1 :-> t2 -> apSustT t1 sust :-> apSustT t2 sust

apareceEn :: Nombre -> Tipo -> Bool
n `apareceEn` s = case s of
                        TNat   -> False
                        TBool  -> False
                        (X n')    -> n == n'
                        t1 :-> t2 -> n `apareceEn` t1 || n `apareceEn` t2

seSustituyeEn :: Nombre -> Sust -> Bool
_ `seSustituyeEn` []           = False
n `seSustituyeEn` ((n', t):ss) = n `apareceEn` t || n `seSustituyeEn` ss

--Realiza la composición de dos sustituciones.
compSust::Sust->Sust->Sust
compSust s1 s2 = s1' ++ s2'
  where
    s1' = map (\(n, t) -> (n, apSustT t s2)) s1
    s2' = filter (\(n, _) -> not $ n `seSustituyeEn` s1) s2

--Unifica dos tipos.
unifica::Tipo->Tipo->[Sust]
unifica TNat  t = case t of
                    TNat -> []
                    X n  -> [[(n, TNat)]]
                    _    -> error "No se pudo unificar."
unifica TBool t = case t of
                    TBool -> []
                    X n   -> [[(n, TBool)]]
                    _     -> error "No se pudo unificar."
unifica (X n)       t = [[(n, t)]]
unifica (t1 :-> t2) t = case t of
                          TNat  -> error "No se pudo unificar."
                          TBool -> error "No se pudo unificar."
                          (X t')        -> [[(t', t1 :-> t2)]]
                          (t1' :-> t2') -> (:[]) $ foldl compSust [] $ unifica t1 t1' ++ unifica t2 t2'


--Unifica una lista de tipos.
unificaConj::[(Tipo,Tipo)]->[Sust]
unificaConj [] = [[]]
unificaConj ((t1,t2):ts) = [compSust s1 s2 | s1 <- unifica t1 t2, s2 <- unificaConj [(apSustT (fst t) s1,apSustT (snd t) s1) | t <- ts]]
