% Tarea 0
% Fabián Heredia Montiel
---
lang: es
---

\newcommand{\eqdef}{\overset{\text{def}}=}
\newcommand{\eqhip}{\overset{\text{hip}}=}

# 1. Suma Naturales

## a. Suma Cero

Pd. $\forall n: \mathbb{N} [n + 0 = n]$

**Base Inductiva**

$0 + 0 \eqdef 0$

**Hipótesis Inductiva**

$n + 0 = n$

**Paso Inductivo**

$Sn + 0 \eqdef S(n + 0) \eqhip Sn$

## b. Suma uno es Sucesor

Pd. $\forall n: \mathbb{N} [n + 1 = Sn]$

**Base Inductiva**

$0 + 1 \eqdef 1 \eqdef S 0$

**Hipótesis Inductiva**

$n + 1 = Sn$

**Paso Inductivo**

$Sn + 1 \eqdef S(n + 1) \eqhip S(Sn)$

# 2. Multiplicación de Naturales

$n*0 := 0$

$n*Sm := n + (n*m)$

## a. Cero por Algo es Cero

Pd. $\forall n : \mathbb{N} [0*n = 0]$

**Base Inductiva**

$0*0 \eqdef 0$

**Hipótesis Inductiva**

$0*n = 0$

**Paso Inductivo**

$0*Sn \eqdef 0 + (0*n) \eqhip 0 + 0 \eqdef 0$

## b. Uno es identidad derecha

Pd. $\forall n : \mathbb{N} [n*1 = n]$

$n*1 \eqdef n*(S 0) \eqdef n + (n * 0) \eqdef n + 0 = n$

# 3. Igualdad

$f(0,0) := \top$

$f(Sn,Sm) := f(n,m)$

$f(n,m) := \bot$

# 4. Función Misteriosa

$$
h(n,m) =
    \begin{cases}
        1           & \text{si} \ m = 0 \\
        h(n*n, m/2) & \text{si} \ m = 2m' \\
        n*h(n, m-1) & \text{si} \ m = 2m' + 1 \\
    \end{cases}
$$

Renombrando y usando notación infija

$$
n^m =
    \begin{cases}
        1           & \text{si} \ m = 0 \\
        (n*n)^{m/2} & \text{si} \ m = 2m' \\
        n*(n^{m-1}) & \text{si} \ m = 2m' + 1 \\
    \end{cases}
$$

La función misteriosa es la exponencial.

# 5. Lista

## a. Concatenar Lista Vacía es Neutro

Pd. $\forall l: l_A [l ++ [] = l**$

**Base Inductiva**

$[] ++ [] \eqdef []$

**Hipótesis Inductiva**

$l ++ [] = l$

**Paso Inductivo**

$(x:l) ++ [] \eqdef x:(l ++ []) \eqhip x:l$

# 6. Reversa de Lista

## a. Definición Recursiva

$rev [] = []$

$rev (x:xs) = (rev xs) ++ [x]$

# 7. Operaciones de Naturales

```haskell
suma :: Nat -> Nat -> Nat
suma n Cero  = n
suma n (S m) = S(suma n m)

prod :: Nat -> Nat -> Nat
prod _ Cero  = Cero
prod n (S m) = suma n (prod n m)

iguales :: Nat -> Nat -> Bool
iguales Cero  Cero  = True
iguales (S n) (S m) = iguales n m
iguales    _     _  = False
```
# 8. Exponencial

```haskell
par :: Nat -> Bool
par Cero = True
par (S m) = no . par $ m

mitad :: Nat -> Nat
mitad Cero = Cero
mitad (S (S n)) = S (mitad n)
mitad (S _) = Cero

h :: Nat -> Nat -> Nat
h _ Cero = S Cero
h n (S m)
  | par (S m) = h (prod n n) (mitad (S m))
  | otherwise = prod n (h n m)
```

# 9. Primero y Ultimo

```haskell
primero :: [a] -> a
primero []     = undefined
primero (x:_) = x

ultimo :: [a] -> a
ultimo     [] = undefined
ultimo    [x] = x
ultimo (_:xs) = ultimo xs

pyu :: [a] -> (a,a)
pyu l = (primero l, ultimo l)
```

# 10. Clona

```haskell
clona :: [Int] -> [Int]
clona [] = []
clona (x:xs) = aux x ++ clona xs
  where aux n = [n | _ <- [1..n]]
```

# 11. Agrupa

```haskell
agrupa :: [Int] -> [[Int]]
agrupa []     = []
agrupa (x:xs) = (x:grupo) : agrupa restante
  where
    repetido y = x == y
    grupo      = takeWhile repetido xs
    restante   = dropWhile repetido xs
```

# 12. Freq

```haskell
freq :: [Int] -> [(Int, Int)]
freq    []  = []
freq (x:xs) = (x, length mismos) : freq restantes
  where
    mismos    = filter (x ==) (x:xs)
    restantes = filter (x /=)    xs
```
