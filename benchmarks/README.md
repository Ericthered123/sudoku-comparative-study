# Benchmarks

Comparación sistemática de los 6 solvers sobre sets de puzzles por dificultad.

## Contenido

| Path | Qué es |
|---|---|
| `comparison.py` | Runner del benchmark: ejecuta cada solver sobre cada puzzle, con timeout de 30 s |
| `generate_puzzles.py` | Generador de puzzles con solución única |
| `puzzles/{easy,medium,hard}/puzzles.txt` | 20 puzzles por dificultad, formato de 81 caracteres (`0` = vacía) |
| `results/` | Salidas: CSV por puzzle, estadísticas agregadas, JSON y figuras PNG |

## Correr el benchmark

```bash
# Todas las dificultades, los 6 solvers
python benchmarks/comparison.py

# Filtrar
python benchmarks/comparison.py --difficulty easy
python benchmarks/comparison.py --max-puzzles 5
python benchmarks/comparison.py --solvers prolog_clp haskell_mrv
```

Requiere `swipl` en PATH y `sudoku-exe` compilado (`cd haskell && stack build`).
Para las figuras: `matplotlib` y `numpy`.

## Salidas en `results/`

- `benchmark_results.csv` — una fila por (solver, puzzle): tiempo, éxito, timeout
- `statistics.csv` / `results.json` — agregados por solver y dificultad
  (media, mediana, min, max, desvío, tasa de éxito)
- `fig1_bar_linear.png`, `fig2_bar_log.png` — medianas comparadas (usar la log)
- `fig3_boxplot.png` — dispersión por solver y dificultad
- `fig4_heatmap_speedup.png` — speedup relativo entre solvers

## Metodología

- Cada solver corre en un **proceso propio** y reporta su tiempo interno
  (excluye startup del proceso — ver `../docs/arquitectura.md`).
- Los solvers corren **secuencialmente** para no competir por CPU.
- Timeout: 30 s por (solver, puzzle); los timeouts cuentan como fallo y quedan
  registrados en el CSV.
- Limitación conocida: una corrida por puzzle. El roadmap
  (`../IMPROVEMENTS.md` §2) contempla N corridas con mediana y conteo de
  backtracks para una comparación independiente de la máquina.
