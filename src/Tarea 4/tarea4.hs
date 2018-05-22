{-
Facultad de Ciencias UNAM - Programación Declarativa 2018-2
Profesor: C. Moisés Vázquez Reyes
Ayudante: Enrique Antonio Bernal Cedillo
-}

module Tarea4 where

import Data.List (intercalate)
import Control.Monad (guard)
import Control.Monad.State

-- Ejercicio 1

primeWheel2 :: Integral a => a -> [a]
primeWheel2 n | n > 2 = 2:[3,5..n]
              | n == 2 = [2]
              | otherwise = []

prime :: Int -> Bool
prime p | p < 2     = False
        | otherwise = not . any divBy $ primeWheel2 (p-1)
  where divBy n = p `mod` n == 0

goldbach :: Int -> [(Int, Int, Int)]
goldbach n = do
  x <- primeWheel2 (n-4)
  y <- primeWheel2 (n-x)
  let z = n-x-y
  guard $ x <= y && y <= z
  guard $ prime x && prime y && prime z
  return (x,y,z)

-- Ejercicio 2

data Arreglo a = Arr (Int -> a) Int

instance Show a => Show (Arreglo a) where
  show (Arr f n) | n < 0     = "Arreglo Malformado"
                 | n == 0    = "∅"
                 | otherwise = "[" ++ intercalate ", " [show $ f i | i <- [0..n-1]] ++ "]"

instance Functor Arreglo where
  fmap f (Arr a n) = Arr (f . a) n

-- Ejercicio 3

minArrState :: Ord a => Arreglo a -> Int -> Int
minArrState array@(Arr f n) i | n > 0 && 0 <= i && i < n = evalState (minArrState' array) (i, i)
                              | otherwise = undefined

minArrState' :: Ord a => Arreglo a -> State (Int, Int) Int
minArrState' array@(Arr f n) = do
  (i, known_min) <- get
  if i < n then
    if f i < f known_min then do
      put (i + 1, i)
      minArrState' array
    else do
      put (i + 1, known_min)
      minArrState' array
  else
    return known_min

selectionSortState :: Ord a => Arreglo a -> Arreglo a
selectionSortState array = evalState selectionSortState' (Arr undefined 0, array)

selectionSortState' :: Ord a => State (Arreglo a, Arreglo a) (Arreglo a)
selectionSortState' = do
  (sorted, unsorted@(Arr g m)) <- get
  if m > 0 then do
    let i = minArrState unsorted 0
    put (cons (g i) sorted, remove i unsorted)
    selectionSortState'
  else
    return sorted

-- Ejercicio 4
length1 :: [Int] -> State Int Int
length1 [] = get
length1 (x:xs) =
  if x == 1 then do
    i <- get
    put (i + 1)
    length1 xs
  else
    length1 xs

-- Codigo general

-- Definiciones Misc para Arreglo

cons :: a -> Arreglo a -> Arreglo a
cons a (Arr f n) = Arr f' (n+1)
  where f' i | i==n      = a
             | otherwise = f i

remove :: Int -> Arreglo a -> Arreglo a
remove i (Arr f n) = Arr f' (n-1)
  where f' j | j == i    = f (n-1)
             | otherwise = f j
