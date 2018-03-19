module Tarea where

data Nat = Cero | S Nat deriving Show

no :: Bool -> Bool
no True  = False
no False = True

iguales :: Nat -> Nat -> Bool
iguales Cero  Cero  = True
iguales (S n) (S m) = iguales n m
iguales    _     _  = False

par :: Nat -> Bool
par Cero = True
par (S m) = no . par $ m



suma :: Nat -> Nat -> Nat
suma n Cero  = n
suma n (S m) = S(suma n m)

prod :: Nat -> Nat -> Nat
prod _ Cero  = Cero
prod n (S m) = suma n (prod n m)

mitad :: Nat -> Nat
mitad Cero = Cero
mitad (S (S n)) = S (mitad n)
mitad (S _) = Cero

h :: Nat -> Nat -> Nat
h _ Cero = S Cero
h n (S m)
  | par (S m) = h (prod n n) (mitad (S m))
  | otherwise = prod n (h n m)



primero :: [a] -> a
primero []     = undefined
primero (x:_) = x

ultimo :: [a] -> a
ultimo     [] = undefined
ultimo    [x] = x
ultimo (_:xs) = ultimo xs

pyu :: [a] -> (a,a)
pyu l = (primero l, ultimo l)



clona :: [Int] -> [Int]
clona [] = []
clona (x:xs) = aux x ++ clona xs
  where aux n = [n | _ <- [1..n]]

agrupa :: [Int] -> [[Int]]
agrupa []     = []
agrupa (x:xs) = (x:grupo) : agrupa restante
  where
    repetido y = x == y
    grupo      = takeWhile repetido xs
    restante   = dropWhile repetido xs

freq :: [Int] -> [(Int, Int)]
freq    []  = []
freq (x:xs) = (x, length mismos) : freq restantes
  where
    mismos    = filter (x ==) (x:xs)
    restantes = filter (x /=)    xs
