# Solvers Prolog

Tres solvers sobre SWI-Prolog, del más explícito al más declarativo. La
explicación algorítmica completa está en [`../docs/algoritmos.md`](../docs/algoritmos.md).

| Archivo | Enfoque | Predicado principal | Representación |
|---|---|---|---|
| `src/sudoku_manual.pl` | Backtracking ingenuo | `sudoku(+Input, -Sol)` | Lista plana de 81, `x` = vacía |
| `src/sudoku_manual_mrv.pl` | Backtracking + heurística MRV | `resolver(+Input, -Sol)` | Lista plana de 81, `x` = vacía |
| `src/sudoku_clp.pl` | CLP(FD): `all_distinct` + `labeling(ff)` | `sudoku(+Matrix)` (in-place) | Matriz 9×9, `_` = vacía |
| `src/tests.pl` | Suite de casos de prueba | — | — |
| `src/ejemplos.pl` | Puzzles de ejemplo y consultas | — | — |

## Uso interactivo

```bash
# CLP(FD)
swipl prolog/src/sudoku_clp.pl
?- ejemplo_facil(S), resolver_e_imprimir(S).

# Backtracking ingenuo
swipl prolog/src/sudoku_manual.pl
?- ejemplo(S), sudoku(S, Sol), imprimir_sudoku_manual(Sol).

# MRV
swipl prolog/src/sudoku_manual_mrv.pl
?- ejemplo_simple(S), resolver_e_imprimir(S).
```

## Cómo los invoca el backend

`backend/app.py` genera un script temporal que consulta el solver, lo resuelve
midiendo con `get_time/1` (wall clock, ~100 ns de resolución en Windows vía
`QueryPerformanceCounter`) e imprime dos líneas: tiempo en ms y solución de 81
dígitos. Los solvers manuales reciben la lista con `x`; el CLP recibe la matriz
con variables libres.

## Notas de diseño

- Los tres comparten el mismo contrato de entrada/salida para que el benchmark
  sea de a pares con las estrategias Haskell equivalentes.
- En `sudoku_manual_mrv.pl` el cálculo de candidatos (`taken_in/2`) inspecciona
  sin unificar variables ajenas — el clásico bug de un MRV en Prolog es contar
  candidatos unificando de más.
- En `sudoku_clp.pl`, `labeling([ff], Vs)` usa first-fail (la variable de menor
  dominio primero); hay variantes comentadas (`ffc`, `bisect`) que quedaron
  como posible benchmark de heurísticas de labeling.
