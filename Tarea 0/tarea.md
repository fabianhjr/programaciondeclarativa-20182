% Tarea 0
% Fabián Heredia Montiel
---
lang: es
---

\newcommand{\eqdef}{\overset{\text{def}}=}
\newcommand{\eqhip}{\overset{\text{hip}}=}

# Suma Naturales

## Suma Cero

Pd. $\forall n: \mathbb{N} [n + 0 = n]$

**Base Inductiva**

$0 + 0 \eqdef 0$

**Hipótesis Inductiva**

$n + 0 = n$

**Paso Inductivo**

$Sn + 0 \eqdef S(n + 0) \eqhip Sn$

## Suma uno es Sucesor

Pd. $\forall n: \mathbb{N} [n + 1 = Sn]$

**Base Inductiva**

$0 + 1 \eqdef 1 \eqdef S 0$

**Hipótesis Inductiva**

$n + 1 = Sn$

**Paso Inductivo**

$Sn + 1 \eqdef S(n + 1) \eqhip S(Sn)$

# Multiplicación de Naturales

$n*0 := 0$

$n*Sm := n + (n*m)$

## Cero por Algo es Cero

Pd. $\forall n : \mathbb{N} [0*n = 0]$

**Base Inductiva**

$0*0 \eqdef 0$

**Hipótesis Inductiva**

$0*n = 0$

**Paso Inductivo**

$0*Sn \eqdef 0 + (0*n) \eqhip 0 + 0 \eqdef 0$

## Uno es identidad derecha

Pd. $\forall n : \mathbb{N} [n*1 = n]$

$n*1 \eqdef n*(S 0) \eqdef n + (n * 0) \eqdef n + 0 = n$

# Igualdad

$f(0,0) := \top$

$f(Sn,Sm) := f(n,m)$

$f(n,m) := \bot$
