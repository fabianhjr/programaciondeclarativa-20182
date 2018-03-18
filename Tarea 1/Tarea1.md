% Tarea 1
% Fabián Heredia Montiel

---
lang: es
header-includes:
    - \usepackage{breqn}
    - \setkeys{breqn}{breakdepth={5}}
---

# Church

Se usa la implementación del ejercicio 5 para hacer las cuentas.

## Calcular $f_1 (5, 0)$ y $f_1 (2, 3)$

### $f_1 (5, 0)$


= $((\lambda n.(\lambda m.(\lambda s.(\lambda z.(((m n) s) z))))) (\lambda s.(\lambda z.(s (s (s (s (s z)))))))) (\lambda s.(\lambda z.z))$ \break
= $(\lambda m.(\lambda s.(\lambda z.(((m (\lambda s.(\lambda z.(s (s (s (s (s z)))))))) s) z)))) (\lambda s.(\lambda z.z))$ \break
= $\lambda s.(\lambda z.((((\lambda s.(\lambda z.z)) (\lambda s.(\lambda z.(s (s (s (s (s z)))))))) s) z))$ \break
= $\lambda s.(\lambda z.(((\lambda z.z) s) z))$ \break
= $\lambda s.(\lambda z.(s z))$ \break
= churchN 1

### $f_1 (2, 3)$

= $((\lambda n.(\lambda m.(\lambda s.(\lambda z.(((m n) s) z))))) (\lambda s.(\lambda z.(s (s z))))) (\lambda s.(\lambda z.(s (s (s z))))))$ \break
= $(\lambda m.(\lambda s.(\lambda z.(((m (\lambda s.(\lambda z.(s (s z))))) s) z)))) (\lambda s.(\lambda z.(s (s (s z)))))$ \break
= $\lambda s.(\lambda z.((((\lambda s.(\lambda z.(s (s (s z))))) (\lambda s.(\lambda z.(s (s z))))) s) z))$ \break
= $\lambda s.(\lambda z.(((\lambda z.((\lambda s.(\lambda z.(s (s z)))) ((\lambda s.(\lambda z.(s (s z)))) ((\lambda s.(\lambda z.(s (s z)))) z)))) s) z))$ \break
= $\lambda s.(\lambda z.(((\lambda s.(\lambda z.(s (s z)))) ((\lambda s.(\lambda z.(s (s z)))) ((\lambda s.(\lambda z.(s (s z)))) s))) z))$ \break
= $\lambda s.(\lambda z.((\lambda z.(((\lambda s.(\lambda z.(s (s z)))) ((\lambda s.(\lambda z.(s (s z)))) s)) (((\lambda s.(\lambda z.(s (s z)))) ((\lambda s.(\lambda z.(s (s z)))) s)) z))) z))$ \break
= $\lambda s.(\lambda z.(((\lambda s.(\lambda z.(s (s z)))) ((\lambda s.(\lambda z.(s (s z)))) s)) (((\lambda s.(\lambda z.(s (s z)))) ((\lambda s.(\lambda z.(s (s z)))) s)) z)))$ \break
= $\lambda s.(\lambda z.((\lambda z.(((\lambda s.(\lambda z.(s (s z)))) s) (((\lambda s.(\lambda z.(s (s z)))) s) z))) (((\lambda s.(\lambda z.(s (s z)))) ((\lambda s.(\lambda z.(s (s z)))) s)) z)))$ \break
= $\lambda s.(\lambda z.(((\lambda s.(\lambda z.(s (s z)))) s) (((\lambda s.(\lambda z.(s (s z)))) s) (((\lambda s.(\lambda z.(s (s z)))) ((\lambda s.(\lambda z.(s (s z)))) s)) z))))$ \break
= $\lambda s.(\lambda z.((\lambda z.(s (s z))) (((\lambda s.(\lambda z.(s (s z)))) s) (((\lambda s.(\lambda z.(s (s z)))) ((\lambda s.(\lambda z.(s (s z)))) s)) z))))$ \break
= $\lambda s.(\lambda z.(s (s (((\lambda s.(\lambda z.(s (s z)))) s) (((\lambda s.(\lambda z.(s (s z)))) ((\lambda s.(\lambda z.(s (s z)))) s)) z)))))$ \break
= $\lambda s.(\lambda z.(s (s ((\lambda z.(s (s z))) (((\lambda s.(\lambda z.(s (s z)))) ((\lambda s.(\lambda z.(s (s z)))) s)) z)))))$ \break
= $\lambda s.(\lambda z.(s (s (s (s (((\lambda s.(\lambda z.(s (s z)))) ((\lambda s.(\lambda z.(s (s z)))) s)) z))))))$ \break
= $\lambda s.(\lambda z.(s (s (s (s ((\lambda z.(((\lambda s.(\lambda z.(s (s z)))) s) (((\lambda s.(\lambda z.(s (s z)))) s) z))) z))))))$ \break
= $\lambda s.(\lambda z.(s (s (s (s (((\lambda s.(\lambda z.(s (s z)))) s) (((\lambda s.(\lambda z.(s (s z)))) s) z)))))))$ \break
= $\lambda s.(\lambda z.(s (s (s (s ((\lambda z.(s (s z))) (((\lambda s.(\lambda z.(s (s z)))) s) z)))))))$ \break
= $\lambda s.(\lambda z.(s (s (s (s (s (s (((\lambda s.(\lambda z.(s (s z)))) s) z))))))))$ \break
= $\lambda s.(\lambda z.(s (s (s (s (s (s ((\lambda z.(s (s z))) z))))))))$ \break
= $\lambda s.(\lambda z.(s (s (s (s (s (s (s (s z)))))))))$ \break
= churchN 8


## Calcular $g_1 (0)$ y $g_1 (3)$

### $g_1 (0)$

= $(\lambda n.(\lambda s.(\lambda z.(((n (\lambda h1.(\lambda h2.(h2 (h1 s))))) (\lambda u.z)) (\lambda u.u))))) (\lambda s.(\lambda z.z))$ \break
= $\lambda s.(\lambda z.((((\lambda s.(\lambda z.z)) (\lambda h1.(\lambda h2.(h2 (h1 s))))) (\lambda u.z)) (\lambda u.u)))$ \break
= $\lambda s.(\lambda z.(((\lambda z.z) (\lambda u.z)) (\lambda u.u)))$ \break
= $\lambda s.(\lambda z.((\lambda u.z) (\lambda u.u)))$ \break
= $\lambda s.(\lambda z.z)$ \break
= churchN 0

### $g_1 (3)$

= $(\lambda n.(\lambda s.(\lambda z.(((n (\lambda h1.(\lambda h2.(h2 (h1 s))))) (\lambda u.z)) (\lambda u.u))))) (\lambda s.(\lambda z.(s (s (s z)))))$ \break
= $\lambda s.(\lambda z.((((\lambda s.(\lambda z.(s (s (s z))))) (\lambda h1.(\lambda h2.(h2 (h1 s))))) (\lambda u.z)) (\lambda u.u)))$ \break
= $\lambda s.(\lambda z.(((\lambda z.((\lambda h1.(\lambda h2.(h2 (h1 s)))) ((\lambda h1.(\lambda h2.(h2 (h1 s)))) ((\lambda h1.(\lambda h2.(h2 (h1 s)))) z)))) (\lambda u.z)) (\lambda u.u)))$ \break
= $\lambda s.(\lambda z.(((\lambda h1.(\lambda h2.(h2 (h1 s)))) ((\lambda h1.(\lambda h2.(h2 (h1 s)))) ((\lambda h1.(\lambda h2.(h2 (h1 s)))) (\lambda u.z)))) (\lambda u.u)))$ \break
= $\lambda s.(\lambda z.((\lambda h2.(h2 (((\lambda h1.(\lambda h2.(h2 (h1 s)))) ((\lambda h1.(\lambda h2.(h2 (h1 s)))) (\lambda u.z))) s))) (\lambda u.u)))$ \break
= $\lambda s.(\lambda z.((\lambda u.u) (((\lambda h1.(\lambda h2.(h2 (h1 s)))) ((\lambda h1.(\lambda h2.(h2 (h1 s)))) (\lambda u.z))) s)))$ \break
= $\lambda s.(\lambda z.(((\lambda h1.(\lambda h2.(h2 (h1 s)))) ((\lambda h1.(\lambda h2.(h2 (h1 s)))) (\lambda u.z))) s))$ \break
= $\lambda s.(\lambda z.((\lambda h2.(h2 (((\lambda h1.(\lambda h2.(h2 (h1 s)))) (\lambda u.z)) s))) s))$ \break
= $\lambda s.(\lambda z.(s (((\lambda h1.(\lambda h2.(h2 (h1 s)))) (\lambda u.z)) s)))$ \break
= $\lambda s.(\lambda z.(s ((\lambda h2.(h2 ((\lambda u.z) s))) s)))$ \break
= $\lambda s.(\lambda z.(s (s ((\lambda u.z) s))))$ \break
= $\lambda s.(\lambda z.(s (s z)))$ \break
= churchN 2

## Calcular $h_1 (1)$ y $h_1 (2)$

### $h_1 (1)$

= $(\lambda n.((\lambda p.(p (\lambda x.(\lambda y.x)))) ((n (\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p))))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))) (\lambda s.(\lambda z.(s z)))$ \break
= $(\lambda p.(p (\lambda x.(\lambda y.x)))) (((\lambda s.(\lambda z.(s z))) (\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p))))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))$ \break
= $(((\lambda s.(\lambda z.(s z))) (\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p))))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))) (\lambda x.(\lambda y.x))$ \break
= $((\lambda z.((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) z)) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))) (\lambda x.(\lambda y.x))$ \break
= $((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))) (\lambda x.(\lambda y.x))$ \break
= $(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))) (\lambda x.(\lambda y.x))$ \break
= $((\lambda y.(\lambda p.((p ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))) y))) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))) (\lambda x.(\lambda y.x))$ \break
= $(\lambda p.((p ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))))) (\lambda x.(\lambda y.x))$ \break
= $((\lambda x.(\lambda y.x)) ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))$ \break
= $(\lambda y.((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))$ \break
= $(\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))$ \break
= $(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))) (\lambda x.(\lambda y.y))$ \break
= $((\lambda y.(\lambda p.((p (\lambda s.(\lambda z.z))) y))) (\lambda s.(\lambda z.z))) (\lambda x.(\lambda y.y))$ \break
= $(\lambda p.((p (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))) (\lambda x.(\lambda y.y))$ \break
= $((\lambda x.(\lambda y.y)) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))$ \break
= $(\lambda y.y) (\lambda s.(\lambda z.z))$ \break
= $\lambda s.(\lambda z.z)$ \break
= churchN 0

### $h_1 (2)$

= $(\lambda n.((\lambda p.(p (\lambda x.(\lambda y.x)))) ((n (\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p))))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))) (\lambda s.(\lambda z.(s (s z))))$ \break
= $(\lambda p.(p (\lambda x.(\lambda y.x)))) (((\lambda s.(\lambda z.(s (s z)))) (\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p))))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))$ \break
= $(((\lambda s.(\lambda z.(s (s z)))) (\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p))))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))) (\lambda x.(\lambda y.x))$ \break
= $((\lambda z.((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) ((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) z))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))) (\lambda x.(\lambda y.x))$ \break
= $((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) ((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))) (\lambda x.(\lambda y.x))$ \break
= $(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) ((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) ((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))))) (\lambda x.(\lambda y.x))$ \break
= $((\lambda y.(\lambda p.((p ((\lambda p.(p (\lambda x.(\lambda y.y)))) ((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))) y))) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) ((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))))) (\lambda x.(\lambda y.x))$ \break
= $(\lambda p.((p ((\lambda p.(p (\lambda x.(\lambda y.y)))) ((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) ((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))))) (\lambda x.(\lambda y.x))$ \break
= $((\lambda x.(\lambda y.x)) ((\lambda p.(p (\lambda x.(\lambda y.y)))) ((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) ((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))))$ \break
= $(\lambda y.((\lambda p.(p (\lambda x.(\lambda y.y)))) ((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) ((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))))$ \break
= $(\lambda p.(p (\lambda x.(\lambda y.y)))) ((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))$ \break
= $((\lambda p.(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) p)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))) (\lambda x.(\lambda y.y))$ \break
= $(((\lambda x.(\lambda y.(\lambda p.((p x) y)))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))) (\lambda x.(\lambda y.y))$ \break
= $((\lambda y.(\lambda p.((p ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))) y))) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))) (\lambda x.(\lambda y.y))$ \break
= $(\lambda p.((p ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))))) (\lambda x.(\lambda y.y))$ \break
= $((\lambda x.(\lambda y.y)) ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))$ \break
= $(\lambda y.y) ((\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))))$ \break
= $(\lambda n.(\lambda s.(\lambda z.(s ((n s) z))))) ((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))))$ \break
= $\lambda s.(\lambda z.(s ((((\lambda p.(p (\lambda x.(\lambda y.y)))) (((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))) s) z)))$ \break
= $\lambda s.(\lambda z.(s ((((((\lambda x.(\lambda y.(\lambda p.((p x) y)))) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))) (\lambda x.(\lambda y.y))) s) z)))$ \break
= $\lambda s.(\lambda z.(s (((((\lambda y.(\lambda p.((p (\lambda s.(\lambda z.z))) y))) (\lambda s.(\lambda z.z))) (\lambda x.(\lambda y.y))) s) z)))$ \break
= $\lambda s.(\lambda z.(s ((((\lambda p.((p (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z)))) (\lambda x.(\lambda y.y))) s) z)))$ \break
= $\lambda s.(\lambda z.(s (((((\lambda x.(\lambda y.y)) (\lambda s.(\lambda z.z))) (\lambda s.(\lambda z.z))) s) z)))$ \break
= $\lambda s.(\lambda z.(s ((((\lambda y.y) (\lambda s.(\lambda z.z))) s) z)))$ \break
= $\lambda s.(\lambda z.(s (((\lambda s.(\lambda z.z)) s) z)))$ \break
= $\lambda s.(\lambda z.(s ((\lambda z.z) z)))$ \break
= $\lambda s.(\lambda z.(s z))$ \break
= churchN 1

## Funciones

Las funciones son:

 - $f_1 (a,b)$ es $a^b$
 - $g_1 (a)$ es \underline{pred}
 - $h_1 (a)$ es \underline{pred}.

# Scott

## Calcular $f_2 (0)$ y  $f_2 (3)$

### $f_2 (0)$

= $(\lambda n.(\lambda x.(\lambda y.(y n)))) (\lambda x.(\lambda y.x))$ \break
= $\lambda x.(\lambda y.(y (\lambda x.(\lambda y.x))))$ \break
= scottN 1

### $f_2 (3)$

= $(\lambda n.(\lambda x.(\lambda y.(y n)))) (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.x)))))))))))$ \break
= $\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.x)))))))))))))$ \break
= scottN 4

## Calcular $g_2 (1)$ y $g_2 (4)$

### $g_2 (1)$

= $(\lambda n.((n (\lambda x.(\lambda y.x))) (\lambda x.x))) (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.x)))))$ \break
= $((\lambda x.(\lambda y.(y (\lambda x.(\lambda y.x))))) (\lambda x.(\lambda y.x))) (\lambda x.x))$ \break
= $(\lambda y.(y (\lambda x.(\lambda y.x)))) (\lambda x.x)$ \break
= $(\lambda x.x) (\lambda x.(\lambda y.x))$ \break
= $\lambda x.(\lambda y.x)$ \break
= scottN 0

### $g_2 (4)$

= $(\lambda n.((n (\lambda x.(\lambda y.x))) (\lambda x.x))) (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.x))))))))))))))$ \break
= $((\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.x)))))))))))))) (\lambda x.(\lambda y.x))) (\lambda x.x)$ \break
= $(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.x))))))))))))) (\lambda x.x)$ \break
= $(\lambda x.x) (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.x)))))))))))$ \break
= $\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.x))))))))))$ \break
= scottN 3

## Calcular $h_2 (0)$ y $h_2 (5)$

### $h_2 (0)$

= $(\lambda n.((n (\lambda x.(\lambda y.x))) (\lambda x.(\lambda x.(\lambda y.y))))) (\lambda x.(\lambda y.x))$ \break
= $((\lambda x.(\lambda y.x)) (\lambda x.(\lambda y.x))) (\lambda x.(\lambda x.(\lambda y.y)))$ \break
= $(\lambda y.(\lambda x.(\lambda y.x))) (\lambda x.(\lambda x.(\lambda y.y)))$ \break
= $\lambda x.(\lambda y.x)$ \break
= \underline{true}

### $h_2 (5)$

= $(\lambda n.((n (\lambda x.(\lambda y.x))) (\lambda x.(\lambda x.(\lambda y.y))))) (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.x)))))))))))))))))$ \break
= $((\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.x))))))))))))))))) (\lambda x.(\lambda y.x))) (\lambda x.(\lambda x.(\lambda y.y)))$ \break
= $(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.x)))))))))))))))) (\lambda x.(\lambda x.(\lambda y.y)))$ \break
= $(\lambda x.(\lambda x.(\lambda y.y))) (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.(y (\lambda x.(\lambda y.x))))))))))))))$ \break
= $\lambda x.(\lambda y.y)$ \break
= \underline{false}

## Funciones

Las funciones son:

 - $f_2$ es \underline{suc}
 - $g_2$ es \underline{pred}
 - $h_2$ es \underline{noCero}

# Combinador-Y

\underline{combinadorY} $:= (\lambda f.((\lambda x.(f \ (x \ x))) \ (\lambda x.(f \ (x \ x)))))$

## Exponente de numerales de Church

\underline{casiChurchExponente} $:= \lambda f.(\lambda \underline{nm}.(((\underline{churchEsCero} \ m) \ (\underline{churchN} \ 1)) \ ((\underline{churchProd} \ n) \ (f \ \underline{nm'}))))$

donde:

 - $n := \underline{fst} \ nm$
 - $m := \underline{snd} \ nm$
 - $\underline{nm'} := (\underline{pair} \ n) \ (\underline{pred} \ m)$

\underline{churchExponente} $:= \underline{combinadorY} \ \underline{casiChurchExponente}$

## Impar de numerales de Scott

\underline{casiScottImpar} $:= \lambda f.(\lambda n.(((\underline{scottEsCero} \ n) \ \underline{false}) \ (\underline{not} \ (f \ (\underline{scottPred} \ n))))$

\underline{scottImpar} $:= \underline{combinadorY} \ \underline{casiScottImpar}$
