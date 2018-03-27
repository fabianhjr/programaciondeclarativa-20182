% Tarea 2
% Fabián Heredia Montiel
---
lang: es
header-includes:
    - \usepackage{prooftrees}
---

# 1. Decir validez usando Tableaux y DPPL

## a. $\{ a \vee b, \neg c \rightarrow  \neg a \} \vDash b \rightarrow \neg c$

### Tableau

\begin{prooftree}
{
to prove={\{ a \vee b, \neg c \rightarrow \neg a \} \vDash b \rightarrow \neg c}
}
[a \vee b, just=As, checked
  [\neg c \rightarrow \neg a, just=As, checked
    [\neg (b \rightarrow \neg c), just={Neg Conc}, checked
      [b, just={Elim $\wedge$}
        [c, just={Elim $\wedge$}
          [a, just=Elim $\vee$
            [c, just=Elim $\vee$]
            [\neg a, just=Elim $\vee$, close]
          ]
          [b, just=Elim $\vee$
            [c, just=Elim $\vee$]
            [\neg a, just=Elim $\vee$]
          ]
]]]]]
\end{prooftree}

### DPLL

$\{ a \vee b, \neg c \rightarrow \neg a \} \vDash b \rightarrow \neg c$

Neg conclusión

$\{ a \vee b, \neg c \rightarrow \neg a, \neg (b \rightarrow \neg c) \}$

Reescribir implicaciones

$\{ a \vee b, c \vee \neg a, b \wedge c \}$

Clausulas

$[[a,b], [c, \neg a], [b], [c]]$

Propagación unitaria de b

$[[c, \neg a], [c]], [b]$

Propagación unitaria de c

$[], [b,c]$

Satisfacible con $[b, c]$

### Conclusión

$[b,c]$ es un modelo que contradice la consecuencia semántica.

## b. $\{ (p \rightarrow r) \vee (\neg s \wedge p), s \rightarrow \neg (p \wedge r) \} \vDash \neg (r \vee \neg s)$

### Tableau

\begin{prooftree}
{
to prove={\{ (p \rightarrow r) \vee (\neg s \wedge p), s \rightarrow \neg (p \wedge r) \} \vDash \neg (r \vee \neg s)}
}
[(p \rightarrow r) \vee (\neg s \wedge p), just=As, checked
  [s \rightarrow \neg (p \wedge r), just=As, checked
    [(r \vee \neg s), just=Neg Conc, checked
      [(p \rightarrow r), just=Elim $\vee$, checked
        [\neg p, just=Elim $\vee$
          [\neg s, just=Elim $\vee$
            [r, just=Elim $\vee$]
            [\neg s, just=Elim $\vee$]
          ]
          [\neg (p \wedge r), just=Elim $\vee$, checked
            [\neg p, just=Elim $\vee$
              [r, just=Elim $\vee$]
              [\neg s, just=Elim $\vee$]
            ]
            [\neg r, just=Elim $\vee$
              [r, just=Elim $\vee$, close]
              [\neg s, just=Elim $\vee$]
            ]
          ]
        ]
        [r, just=Elim $\vee$
          [\neg s, just=Elim $\vee$
            [r, just=Elim $\vee$]
            [\neg s, just=Elim $\vee$] 
          ]
          [\neg (p \wedge r), just=Elim $\vee$, checked
            [\neg p, just=Elim $\vee$
              [r, just=Elim $\vee$]
              [\neg s, just=Elim $\vee$]
            ]
            [\neg r, just=Elim $\vee$
              [r, just=Elim $\vee$, close]
              [\neg s, just=Elim $\vee$]
            ]
          ]
        ]
      ]
      [(\neg s \wedge p), just=Elim $\vee$, checked
        [\neg s, just=Elim $\wedge$
          [p, just=Elim $\wedge$
            [\neg s, just=Elim $\vee$
              [r, just=Elim $\vee$]
              [\neg s, just=Elim $\vee$]
            ]
            [\neg (p \wedge r), just=Elim $\vee$, checked
              [\neg p, just=Elim $\vee$, close]
              [\neg r, just=Elim $\vee$
                [r, just=Elim $\vee$, close]
                [\neg s, just=Elim $\vee$]
              ]
            ]
      ]]]
]]]
\end{prooftree}

### DPLL

$\{ (p \rightarrow r) \vee (\neg s \wedge p), s \rightarrow \neg (p \wedge r) \} \vDash \neg (r \vee \neg s)$

Neg Conclusión

$\{ (p \rightarrow r) \vee (\neg s \wedge p), s \rightarrow \neg (p \wedge r), (r \vee \neg s) \}$

Reescribir Implicaciones

$\{ (\neg p \vee r) \vee (\neg s \wedge p), \neg s \vee (\neg p \vee \neg r), (r \vee \neg s) \}$

Reescribir a Clausulas

$[[\neg p, r, p], [\neg p, r \neg s], [\neg s, \neg p, \neg r], [r, \neg s]]$

Levantando literal pura: $\neg s$

$[[\neg p, r, p]]$

Obs: la única clausula es una tautología

Satisfacible con $[\neg s]$

### Conclusión

$[\neg s]$ es un modelo que contradice la consecuencia semántica

## $\{ (s \rightarrow p) \vee (t \rightarrow q) \} \vDash (s \rightarrow q) \vee (t \rightarrow p)$

### Tableau

\begin{prooftree}
{
to prove={\{ (s \rightarrow p) \vee (t \rightarrow q) \} \vDash (s \rightarrow q) \vee (t \rightarrow p)}
}
[(s \rightarrow p) \vee (t \rightarrow q), just=As, checked
  [\neg (s \rightarrow q) \wedge \neg (t \rightarrow p), just=Neg conc, checked
    [\neg (s \rightarrow q), just=Elim $\wedge$, checked
      [\neg (t \rightarrow p), just=Elim $\wedge$, checked
        [s, just=Elim $\wedge$
          [\neg q, just=Elim $\wedge$
            [t, just=Elim $\wedge$
              [\neg p, just=Elim $\wedge$
                [(s \rightarrow p), just=Elim $\vee$, checked
                  [\neg s, just=Elim $\vee$, close]
                  [p, just=Elim $\vee$, close]
                ]
                [(t \rightarrow q), just=Elim $\vee$, checked
                  [\neg t, just=Elim $\vee$, close]
                  [q, just=Elim $\vee$, close]
                ]
]]]]]]]]
\end{prooftree}

### DPLL

$\{ (s \rightarrow p) \vee (t \rightarrow q) \} \vDash (s \rightarrow q) \vee (t \rightarrow p)$

Neg conclusión

$\{ (s \rightarrow p) \vee (t \rightarrow q), \neg ((s \rightarrow q) \vee (t \rightarrow p)) \}$

Reescribiendo

$\{ (\neg s \vee p) \vee (\neg t \vee q), \neg (s \rightarrow q) \wedge \neg (t \rightarrow p) \}$

Reescribiendo

$\{ (\neg s \vee p) \vee (\neg t \vee q), (s \wedge \neg q) \wedge (t \wedge \neg p) \}$

Clausulas

$[[\neg s, p, \neg t, q], [s], [\neg q], [t], [\neg p]]$

Propagación unitaria de $s$

$[[p, \neg t, q], [\neg q], [t], [\neg p]]$

Propagación unitaria de $\neg q$

$[[p, \neg t], [t], [\neg p]]$

Propagación unitaria de $t$

$[[p], [\neg p]]$

Propagación unitaria de $p$

$[[]]$

Insatisfacible

### Conclusión

Tanto Tableau como DPLL muestran que negar la conclusión es insatisfacible por lo que se da la consecuencia semántica.

## $\{ p \wedge q, r \wedge \neg s, q \rightarrow p \rightarrow t, t \rightarrow r \rightarrow (s \vee w) \} \vDash w$

### Tableau

\begin{prooftree}
{
to prove={\{ p \wedge q, r \wedge \neg s, q \rightarrow p \rightarrow t, t \rightarrow r \rightarrow (s \vee w) \} \vDash w}
}
[p \wedge q, just=As, checked
  [r \wedge \neg s, just=As, checked
    [q \rightarrow p \rightarrow t, just=As, checked
      [t \rightarrow r \rightarrow (s \vee w), just=As, checked
        [\neg w, just=Neg conc
          [p, just=Elim $\wedge$
            [q, just=Elim $\wedge$
              [r, just=Elim $\wedge$
                [\neg s, just=Elim $\wedge$
                  [\neg q, just=Elim $\vee$, close]
                  [p \rightarrow t, just=Elim $\vee$, checked
                    [\neg p, just=Elim $\vee$, close]
                    [t, just=Elim $\vee$
                      [\neg t, just=Elim $\vee$, close]
                      [r \rightarrow (s \vee w), just=Elim $\vee$, checked
                        [\neg r, just=Elim $\vee$, close]
                        [s \vee w, just=Elim $\vee$, checked
                          [s, just=Elim $\vee$, close]
                          [w, just=Elim $\vee$, close]
                        ]
                      ]
                    ]
                  ]
]]]]]]]]]
\end{prooftree}

### DPLL

$\{ p \wedge q, r \wedge \neg s, q \rightarrow p \rightarrow t, t \rightarrow r \rightarrow (s \vee w) \} \vDash w$

Neg conclusión

$\{ p \wedge q, r \wedge \neg s, q \rightarrow p \rightarrow t, t \rightarrow r \rightarrow (s \vee w), \neg w \}$

Reescribir

$\{ p \wedge q, r \wedge \neg s, \neg q \vee (\neg p \vee t), \neg t \vee (\neg r \vee (s \vee w)), \neg w \}$

Clausulas

$[[p], [q], [r], [\neg s], [\neg q, \neg p, t], [\neg t, \neg r, s, w], [\neg w]]$

Propagación unitaria de $p$

$[[q], [r], [\neg s], [\neg q, t], [\neg t, \neg r, s, w], [\neg w]]$

Propagación unitaria de $q$

$[[r], [\neg s], [t], [\neg t, \neg r, s, w], [\neg w]]$

Propagación unitaria de $r$

$[[\neg s], [t], [\neg t, s, w], [\neg w]]$

Propagación unitaria de $\neg s$

$[[t], [\neg t, w], [\neg w]]$

Propagación unitaria de $t$

$[[w], [\neg w]]$

Propagación unitaria de $w$

$[[]]$

INSAT

### Conclusión

Tanto Tableau como DPLL muestran que negar la conclusión es insatisfacible por lo que se da la consecuencia semántica.

# Traducir a Calculo de Proposiciones y decidir por DPLL si son correctos.

## Enunciado 1

Llaves:

 - $L$: Llovio
 - $P$: Es primavera
 - $I$: Es invierno
 - $R$: Hay rios
 - $GA$: La gente usa gorros azules

Traducción: $\{ L \rightarrow (P \vee I), R \rightarrow L, P \rightarrow GA. \neg GA \wedge R \} \vDash P$

### DPLL

$\{ L \rightarrow (P \vee I), R \rightarrow L, P \rightarrow GA, \neg GA \wedge R \} \vDash P$

Neg conclusión

$\{ L \rightarrow (P \vee I), R \rightarrow L, P \rightarrow GA, \neg GA \wedge R, \neg P \}$

Reescribir

$\{ \neg L \vee (P \vee I), \neg R \vee L, \neg P \vee GA, \neg GA \wedge R, \neg P \}$

Clausulas

$[[\neg L, P, I], [\neg R, L], [\neg P, GA], [\neg GA], [R], [\neg P]]$

Propagación unitaria de $R$

$[[\neg L, P, I], [L], [\neg P, GA], [\neg GA], [\neg P]]$

Propagación unitaria de $L$

$[[P, I], [\neg P, GA], [\neg GA], [\neg P]]$

Propagación unitaria de $\neg GA$

$[[P, I], [\neg P], [\neg P]]$

Propagación unitaria de $\neg P$

$[[I]]$

Propagación unitaria de $I$

$[]$

Satisfacible con: $[R, L, \neg GA, \neg P, I]$

### Conclusión

Como se puede satisfacer la negación de la consecuencia semántica, el razonamiento es incorrecto.

## Enunciado 2

Llaves:

 - $M$: Le gusta Maluma
 - $C$: Le gusta Children Garden
 - $K$: Le gusta Kalimba

Traducción: $\{ M \vee C \vee K, (M \wedge \neg K) \rightarrow C, (M \wedge K) \vee (\neg M \wedge \neg K), C \rightarrow M \}$

### DPLL

$\{ M \vee C \vee K, (M \wedge \neg K) \rightarrow C, (M \wedge K) \vee (\neg M \wedge \neg K), C \rightarrow M \}$

Reescribiendo

$\{ M \vee C \vee K, (\neg M \vee K) \vee C, (M \vee (\neg M \wedge \neg K)) \wedge (K \vee (\neg M \wedge \neg K)), \neg C \vee M \}$

Reescribiendo

$\{ M \vee C \vee K, (\neg M \vee K) \vee C, ((M \vee \neg M) \wedge (M \vee \neg K)) \wedge ((K \vee \neg M) \wedge (K \vee \neg K)), \neg C \vee M \}$

Clausulas

$[[M, C, K], [\neg M, K, C], [M, \neg M], [M, \neg K], [K, \neg M], [K, \neg K], [\neg C, M]]$

Levantar Rama 1: $M$

$[[K, C], [K], [K, \neg K]]$

Propagación unitaria de $K$

$[]$

Satisfacible con $[M, K]$


### Conclusión

Se puede satisfacer todo lo que dijo Israel.
