% Tarea 1
% Fabián Heredia Montiel

# Church

Se usa la implementación del ejercicio 5 para hacer las cuentas.

## Calcular $f_1 (5, 0)$ y $f_1 (2, 3)$

$f_1 (5, 0)$


= ($\lambda$n.$\lambda$m.$\lambda$s.$\lambda$z.((m n)s)z ($\lambda$s.$\lambda$z.s(s(s(s(s z)))))) ($\lambda$s.$\lambda$z.z) \break
= ($\lambda$m.$\lambda$s.$\lambda$z.((m ($\lambda$s.$\lambda$z.s(s(s(s(s z))))))s)z) ($\lambda$s.$\lambda$z.z) \break
= ($\lambda$s.$\lambda$z.((($\lambda$s.$\lambda$z.z) ($\lambda$s.$\lambda$z.s(s(s(s(s z))))))s)z) \break
= ($\lambda$s.$\lambda$z.(($\lambda$z.z)s)z) \break
= ($\lambda$s.$\lambda$z.(s)z) \break
= 1


$f_1 2 3$

= (($\lambda$n.$\lambda$m.$\lambda$s.$\lambda$z.((m n)s)z) $\lambda$s.$\lambda$z.s(s z)) ($\lambda$s.$\lambda$z.s(s(s z))) \break
= ($\lambda$m.$\lambda$s.$\lambda$z.((m ($\lambda$s.$\lambda$z.s(s z)))s)z) ($\lambda$s.$\lambda$z.s(s(s z))) \break
= $\lambda$s.$\lambda$z.((($\lambda$s.$\lambda$z.s(s(s z))) ($\lambda$s.$\lambda$z.s(s z)))s)z \break
= $\lambda$s.$\lambda$z.(($\lambda$s.$\lambda$z.($\lambda$s.$\lambda$z.s(s z))(($\lambda$s.$\lambda$z.s(s z))(($\lambda$s.$\lambda$z.s(s z)) z)))s)z \break
= $\lambda$s.$\lambda$z.($\lambda$s.$\lambda$z.s(s z))(($\lambda$s.$\lambda$z.s(s z))(($\lambda$s.$\lambda$z.s(s z)) z)) \break
= $\lambda$s.$\lambda$z.($\lambda$s.$\lambda$z.s(s z))($\lambda$s.$\lambda$z.s(s z))($\lambda$z.z(z z)) \break
= $\lambda$s.$\lambda$z.($\lambda$s.$\lambda$z.s(s z))$\lambda$z.($\lambda$z.z(z z))(($\lambda$z.z(z z)) z) \break
.... \break
= $\lambda$s.$\lambda$z.s(s(s(s(s(s(s(s z))))))) \break
= 8


## Calcular $g_1 (0)$ y $g_1 (3)$

$g_1$ 0

= $\lambda$s.$\lambda$z.z \break
= 0

$g_1$ 3

= $\lambda$s.$\lambda$z.s(s z)) \break
= 2

## Calcular $h_1 (1)$ y $h_1 (2)$

$h_1$ 1

= $\lambda$s.$\lambda$z.z \break
= 0

$h_1$ 2

= $\lambda$s.$\lambda$z'.s z' \break
= 1

## Funciones

Las funciones son $f_1 (a,b)$ es $a^b$ y tanto $g_1 (a)$ $h_1 (a)$ son \underline{pred}.


# Scott

## Calcular $f_2 (0)$ y  $f_2 (3)$

$f_2 (0)$

= $\lambda$x.$\lambda$y.y($\lambda$x.$\lambda$y.x) \break
= 1

$f_2 (3)$

= $\lambda$x.$\lambda$y.y($\lambda$x.$\lambda$y.y($\lambda$x.$\lambda$y.y($\lambda$x.$\lambda$y.y($\lambda$x.$\lambda$y.x)))) \break
= 4

## Calcular $g_2 (1)$ y $g_2 (4)$

$g_2 (1)$

= $\lambda$x.$\lambda$y.x \break
= 0

$g_2 (4)$

= $\lambda$x.$\lambda$y.y($\lambda$x.$\lambda$y.y($\lambda$x.$\lambda$y.y($\lambda$x.$\lambda$y.x))) \break
= 3

$h2 (0)$
= $\lambda$x.$\lambda$y.x \break
= True

$h2 (5)$
= $\lambda$x.$\lambda$y.y \break
= False

## Funciones

Las funciones $f_2$ y $g_2$ son \underline{suc} y \underline{pred} respectivamente, mientras que $h_2$ es \underline{noCero}

# Combinador-Y
