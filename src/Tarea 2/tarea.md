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

# Traducir a Calculo de Proposiciones y decidir por DPLL si son correctos.

## Enunciado 1

Llaves:

 - $L$: Llovio
 - $P$: Es primavera
 - $I$: Es invierno
 - $R$: Hay rios
 - $GA$: La gente usa gorros azules

Traducción: $\{ L \rightarrow (P \vee I), R \rightarrow L, P \rightarrow GA. \neg GA \wedge R \} \vDash P$

## Enunciado 2

Llaves:

 - $M$: Le gusta Maluma
 - $C$: Le gusta Children Garden
 - $K$: Le gusta Kalimba

Traducción: $\{ M \vee C \vee K, (M \wedge \neg K) \rightarrow C, (M \wedge K) \vee (\neg M \wedge \neg K), C \rightarrow M \}$
