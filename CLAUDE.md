# CLAUDE.md — Sudoku Comparative Study

## Descripción del proyecto

Estudio comparativo de solvers de Sudoku implementados en **Prolog** (3 variantes) y **Haskell** (3 variantes), desarrollado como proyecto final de _Programación Lógica y Funcional_ en la **UNNOBA** (2026).

Autores: Eric Doyle y Bruno Lodeiro.

El proyecto incluye una **interfaz web** que permite ingresar un puzzle, resolverlo con los 6 solvers simultáneamente y comparar tiempos con gráficos.

---

## Estructura de directorios

```
sudoku-comparative-study/
├── prolog/
│   └── src/
│       ├── sudoku_clp.pl          # Solver CLP(FD) — Constraint Logic Programming
│       ├── sudoku_manual.pl       # Solver backtracking naive
│       └── sudoku_manual_mrv.pl   # Solver backtracking con heurística MRV
├── haskell/
│   ├── app/Main.hs                # CLI entry point
│   └── src/
│       ├── Solver.hs              # Algoritmos: FirstEmpty, MRV, PropagationMRV
│       ├── Types.hs               # Tipos: Board, Cell, Position, SolveStrategy
│       ├── Utils.hs               # parseBoard, boardToString, timeIt, ejemplos
│       └── Sudoku.hs              # Re-export facade
├── benchmarks/
│   ├── comparison.py              # Benchmark comparativo (Python)
│   ├── generate_puzzles.py        # Generador de puzzles
│   └── puzzles/
│       ├── easy/puzzles.txt       # 20 puzzles fáciles
│       ├── medium/puzzles.txt     # 20 puzzles medios
│       └── hard/puzzles.txt       # 20 puzzles difíciles
├── backend/
│   └── app.py                     # Flask API (POST /api/solve, GET /api/puzzles)
├── frontend/
│   └── index.html                 # UI single-file (HTML/CSS/JS puro)
├── docs/                          # Documentación adicional
└── CLAUDE.md                      # Este archivo
```

---

## Formato de puzzle

Todos los puzzles usan el formato de **81 caracteres** en una sola línea:
- `1-9` = celda con valor fijo (pista)
- `0` o `.` = celda vacía

Ejemplo:
```
003020600900305001001806400008102900700000008006708200002609500800203009005010300
```

---

## Solvers disponibles

### Prolog (SWI-Prolog)

| Solver | Archivo | Predicado |
|--------|---------|-----------|
| Naive Backtracking | `sudoku_manual.pl` | `sudoku(+Input, -Sol)` |
| MRV Heuristic | `sudoku_manual_mrv.pl` | `resolver(+Input, -Sol)` |
| CLP(FD) | `sudoku_clp.pl` | `sudoku(+Matrix)` (in-place) |

Los solvers Naive y MRV reciben una lista de 81 elementos con `x` para celdas vacías.
El solver CLP recibe una matriz 9×9 con variables Prolog para celdas vacías.

### Haskell

| Estrategia | Arg CLI | Descripción |
|-----------|---------|-------------|
| `FirstEmpty` | `fe` | Backtracking simple |
| `MostConstrained` | `mrv` | Heurística MRV |
| `PropagationMRV` | `pmrv` | MRV + propagación de restricciones |

---

## Comandos útiles

### Haskell — compilar

```bash
cd haskell
stack build
```

### Haskell — usar el solver (modos)

```bash
# Modo interactivo
sudoku-exe

# Resolver ejemplo predefinido
sudoku-exe --example easy
sudoku-exe --example medium
sudoku-exe --example hard

# Benchmark interno
sudoku-exe --benchmark

# Modo externo (usado por Python/backend) — imprime solo tiempo en segundos
sudoku-exe --bench-external fe   <puzzle81>
sudoku-exe --bench-external mrv  <puzzle81>
sudoku-exe --bench-external pmrv <puzzle81>

# Modo solve (usado por backend) — imprime tiempo\nsolución
sudoku-exe --solve fe   <puzzle81>
sudoku-exe --solve mrv  <puzzle81>
sudoku-exe --solve pmrv <puzzle81>
```

### Prolog — uso interactivo (SWI-Prolog)

```bash
# CLP(FD)
swipl prolog/src/sudoku_clp.pl
?- ejemplo_facil(S), resolver_e_imprimir(S).

# Naive BT
swipl prolog/src/sudoku_manual.pl
?- ejemplo(S), sudoku(S, Sol), imprimir_sudoku_manual(Sol).

# MRV
swipl prolog/src/sudoku_manual_mrv.pl
?- ejemplo_simple(S), resolver_e_imprimir(S).
```

### Benchmark comparativo

```bash
# Todas las dificultades
python benchmarks/comparison.py

# Solo puzzles fáciles
python benchmarks/comparison.py --difficulty easy

# Limitar puzzles por dificultad
python benchmarks/comparison.py --max-puzzles 5

# Solo algunos solvers
python benchmarks/comparison.py --solvers prolog_naive prolog_clp haskell_mrv
```

Resultados exportados en `benchmarks/results/`:
- `benchmark_results.csv` — datos por puzzle
- `benchmark_stats.json` — estadísticas agregadas
- `*.png` — gráficos comparativos

---

## Sistema web (backend + frontend)

### Iniciar el backend (Flask API)

```bash
# Instalar dependencias (solo primera vez)
pip install flask flask-cors

# Iniciar
python backend/app.py
# Corre en http://localhost:5000

# Tests del backend (no requieren swipl ni stack)
python -m pytest backend/test_app.py
```

#### Rutas disponibles:

| Método | Ruta | Descripción |
|--------|------|-------------|
| `GET` | `/api/health` | Estado de swipl y haskell-exe |
| `GET` | `/api/puzzles?difficulty=easy` | Lista de puzzles (easy/medium/hard) |
| `POST` | `/api/solve` | Resolver puzzle con los 6 solvers |
| `POST` | `/api/solve_one` | Resolver puzzle con un solo solver (`{"puzzle": "...", "solver": "prolog_clp"}`) — usado por el frontend para mostrar resultados en vivo |

Ejemplo de uso:
```bash
curl http://localhost:5000/api/health
curl "http://localhost:5000/api/puzzles?difficulty=easy"
curl -X POST http://localhost:5000/api/solve \
     -H "Content-Type: application/json" \
     -d '{"puzzle":"003020600900305001001806400008102900700000008006708200002609500800203009005010300"}'
```

### Abrir el frontend

```bash
# Opción A — abrir directamente en el browser
start frontend/index.html     # Windows
open frontend/index.html      # macOS

# Opción B — servir con Python (evita restricciones file://)
python -m http.server 8080 --directory frontend
# luego abrir http://localhost:8080
```

---

## Dependencias

### Python
- `flask` + `flask-cors` (solo para el backend web)
- Estándar: `re`, `subprocess`, `tempfile`, `statistics`, `pathlib`
- Para benchmarks: `matplotlib`, `numpy` (si se generan gráficos)

### Haskell (stack)
- `array`, `clock` (System.Clock para timing), `printf`
- Ver `haskell/package.yaml` para la lista completa

### Sistema
- **SWI-Prolog** (`swipl`) en el PATH
- **Stack** (Haskell build tool)

---

## Notas de implementación

### Timing
- Prolog: `get_time/1` → Unix timestamp float → diferencia × 1000 → ms
  - Usa QueryPerformanceCounter en Windows (~100 ns resolución)
  - `statistics(cputime/walltime)` descartado: resolución 15.625 ms en Windows
- Haskell: `getTime Monotonic` (System.Clock) → nanosegundos → ÷ 1e9 → segundos
  - También usa QueryPerformanceCounter en Windows
  - `getCPUTime` descartado por misma razón que Prolog

### Puzzle output del backend
El backend llama a los solvers con un modo que devuelve **dos líneas**:
1. Tiempo de ejecución (ms para Prolog, segundos para Haskell)
2. Solución como 81 dígitos

Los runners del benchmark original (`comparison.py`) no se ven afectados porque solo leen la primera línea.
