{-
Facultad de Ciencias UNAM - Programación Declarativa 2018-2
Profesor: C. Moisés Vázquez Reyes
Ayudante: Enrique Antonio Bernal Cedillo
-}

module Unificacion where

import Data.Maybe(fromMaybe)

infixr :-> {- Así, el poderador ':->' asocia a la derecha. -}
type Nombre = String

-- Categoría de tipos.
data Tipo = TNat | TBool | X Nombre | Tipo :-> Tipo deriving Eq


instance Show Tipo where
     show t = case t of
            TNat      -> "ℕ"
            TBool     -> "𝔹"
            X name    -> name
            t1 :-> t2 -> "(" ++ show t1 ++ ")" ++ "->" ++ "(" ++ show t2 ++ ")"


--Una sustitución es un conjunto de la forma [(xi, Ti)]
type Sust = [(Nombre, Tipo)]


--Elimina sustituciones de la forma [X:=X] en una sustitución.
simpSust::Sust->Sust
simpSust = filter (\(xi, ti) -> show xi /= show ti)

--Realiza la composición de dos sustituciones.
compSust::Sust->Sust->Sust
compSust s1 s2 = s1 ++ s2


--Aplica una sustitución a un tipo.
apSustT::Tipo->Sust->Tipo
apSustT (X n) sust = fromMaybe (X n) res
  where res = lookup n sust
apSustT (t1 :-> t2) sust = apSustT t1 sust :-> apSustT t2 sust
apSustT TNat _ = TNat
apSustT TBool _ = TBool

--Unifica dos tipos.
unifica::Tipo->Tipo->[Sust]
unifica = error "Te toca"


--Unifica una lista de tipos.
unificaConj::[(Tipo,Tipo)]->[Sust]
unificaConj [] = [[]]
unificaConj ((t1,t2):ts) = [compSust s1 s2 | s1 <- unifica t1 t2, s2 <- unificaConj [(apSustT (fst t) s1,apSustT (snd t) s1) | t <- ts]]
