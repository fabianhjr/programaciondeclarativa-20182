module Tarea where

import Control.Monad (guard)
import Data.List (intercalate)

-- Ejercicio 1

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

data Arreglo a = Arr (Integer -> a) Integer

instance Show a => Show (Arreglo a) where
  show (Arr f n) | n < 0     = "Arreglo Malformado"
                 | n == 0    = "∅"
                 | otherwise = "[" ++ intercalate ", " [show $ f i | i <- [0..n-1]] ++ "]"

instance Functor Arreglo where
  fmap f (Arr a n) = Arr (f . a) n

-- Ejercicio 3

-- Ejercicio 4
