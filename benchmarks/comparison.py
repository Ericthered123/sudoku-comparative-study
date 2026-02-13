
"""
╔══════════════════════════════════════════════════════════════════════╗
║  BENCHMARK COMPARATIVO - SUDOKU SOLVERS                              ║
║  Prolog (Naive, MRV, CLP(FD)) vs Haskell (FirstEmpty, MRV, PropMRV) ║
╚══════════════════════════════════════════════════════════════════════╝

Autores: Eric Doyle y Bruno Lodeiro
Fecha:   Febrero 2026
Materia: Programación Lógica y Funcional

Metodología:
  - RUNS_PER_PUZZLE ejecuciones por solver×puzzle → mediana como métrica central
  - Calentamiento (warm-up) descartado antes de medir
  - Tiempo de carga de intérprete excluido (--goal / --bench-external)
  - Puzzles en formato de 81 caracteres (0/. = celda vacía)
  - Resultados exportados en CSV, JSON y PNG
"""

import subprocess
import time
import csv
import os
import sys
import json
import statistics
import argparse
from pathlib import Path
from dataclasses import dataclass, asdict, field
from typing import List, Dict, Optional, Callable

# ═══════════════════════════════════════════════════════════════════════
# RUTAS DEL PROYECTO
# ═══════════════════════════════════════════════════════════════════════

PROJECT_ROOT  = Path(__file__).parent.parent
PUZZLES_DIR   = PROJECT_ROOT / "benchmarks" / "puzzles"
RESULTS_DIR   = PROJECT_ROOT / "benchmarks" / "results"
PROLOG_SRC    = PROJECT_ROOT / "prolog" / "src"
HASKELL_DIR   = PROJECT_ROOT / "haskell"

RESULTS_DIR.mkdir(parents=True, exist_ok=True)

# ═══════════════════════════════════════════════════════════════════════
# PARÁMETROS GLOBALES
# ═══════════════════════════════════════════════════════════════════════

TIMEOUT          = 120     # segundos por ejecución individual
RUNS_PER_PUZZLE  = 5       # ejecuciones por solver×puzzle (mediana)
WARMUP_RUNS      = 1       # ejecuciones de calentamiento (descartadas)

# ═══════════════════════════════════════════════════════════════════════
# COLORES ANSI
# ═══════════════════════════════════════════════════════════════════════

class C:
    HEADER = '\033[95m'; BLUE  = '\033[94m'; CYAN  = '\033[96m'
    GREEN  = '\033[92m'; YELLOW= '\033[93m'; RED   = '\033[91m'
    ENDC   = '\033[0m';  BOLD  = '\033[1m';  DIM   = '\033[2m'

def ph(t): print(f"\n{C.HEADER}{'═'*70}\n{C.BOLD}{t:^70}{C.ENDC}{C.HEADER}\n{'═'*70}{C.ENDC}\n")
def ok(t):  print(f"{C.GREEN}✓ {t}{C.ENDC}")
def err(t): print(f"{C.RED}✗ {t}{C.ENDC}")
def warn(t):print(f"{C.YELLOW}⚠ {t}{C.ENDC}")
def info(t):print(f"{C.CYAN}→ {t}{C.ENDC}")

# ═══════════════════════════════════════════════════════════════════════
# ESTRUCTURAS DE DATOS
# ═══════════════════════════════════════════════════════════════════════

@dataclass
class BenchmarkResult:
    solver:      str
    difficulty:  str
    puzzle_num:  int
    puzzle:      str
    time_ms:     float
    success:     bool
    timeout:     bool
    error:       Optional[str] = None
    all_times:   List[float]   = field(default_factory=list)

@dataclass
class AggregatedStats:
    solver:       str
    difficulty:   str
    count:        int
    mean_ms:      float
    median_ms:    float
    min_ms:       float
    max_ms:       float
    stddev_ms:    float
    success_rate: float
    timeout_count:int

# ═══════════════════════════════════════════════════════════════════════
# CARGA DE PUZZLES
# ═══════════════════════════════════════════════════════════════════════

def load_puzzles(difficulty: str) -> List[str]:
    """Carga puzzles de archivo; cada línea es una cadena de 81 caracteres."""
    path = PUZZLES_DIR / difficulty / "puzzles.txt"
    if not path.exists():
        err(f"Archivo no encontrado: {path}")
        return []
    with open(path) as f:
        puzzles = [l.strip() for l in f if len(l.strip()) == 81]
    info(f"Cargados {len(puzzles)} puzzles [{difficulty}]")
    return puzzles

# ═══════════════════════════════════════════════════════════════════════
# HELPERS
# ═══════════════════════════════════════════════════════════════════════

def puzzle_to_prolog_list(puzzle: str) -> str:
    """Convierte cadena de 81 chars en lista Prolog con 'x' para vacíos."""
    tokens = ['x' if c in '0.' else c for c in puzzle]
    return '[' + ','.join(tokens) + ']'

def puzzle_to_prolog_matrix(puzzle: str) -> str:
    """
    Convierte cadena de 81 chars en matriz 9×9 Prolog con variables nombradas.

    Usa variables con nombre único (V00..V88) para celdas vacías.
    - '_' crea variables anónimas independientes → CLP(FD) no puede propagar.
    - '0' requiere normalizar_celda/1 que no puede desinstanciar un entero.
    - Variables nombradas V00..V88 son variables Prolog reales que CLP(FD)
      constrine y propaga correctamente entre filas/columnas/bloques.
    """
    rows = []
    for r in range(9):
        row = []
        for c in range(9):
            ch = puzzle[r*9 + c]
            row.append(f'V{r}{c}' if ch in '0.' else ch)
        rows.append('[' + ','.join(row) + ']')
    return '[' + ','.join(rows) + ']'

def run_process(cmd: List[str], timeout: int) -> tuple[bool, str, str, bool]:
    """
    Ejecuta un proceso externo.
    Retorna (success, stdout, stderr, timed_out).
    """
    try:
        r = subprocess.run(
            cmd, capture_output=True, text=True, timeout=timeout
        )
        return r.returncode == 0, r.stdout.strip(), r.stderr.strip(), False
    except subprocess.TimeoutExpired:
        return False, "", "TIMEOUT", True
    except FileNotFoundError as e:
        return False, "", str(e), False

# ═══════════════════════════════════════════════════════════════════════
# RUNNERS PROLOG
# ═══════════════════════════════════════════════════════════════════════
# En Windows, pasar código Prolog por -g es frágil (límite de longitud
# de argumento, backslashes en rutas, comillas anidadas).
# Solución: escribir un archivo .pl temporal y ejecutarlo con swipl.
# ═══════════════════════════════════════════════════════════════════════

import tempfile

def _make_prolog_script_naive(puzzle: str, src: Path) -> str:
    """Script Prolog completo para el solver Naive."""
    pl_list = puzzle_to_prolog_list(puzzle)
    src_posix = src.as_posix()
    return f""":- use_module(library(lists)).
:- consult('{src_posix}').
:- initialization(main, main).
main :-
    Input = {pl_list},
    statistics(walltime, [_, _]),
    (sudoku(Input, _) -> true ; true),
    statistics(walltime, [_, T]),
    format('~w~n', [T]),
    halt.
"""

def _make_prolog_script_mrv(puzzle: str, src: Path) -> str:
    """Script Prolog completo para el solver MRV."""
    pl_list = puzzle_to_prolog_list(puzzle)
    src_posix = src.as_posix()
    return f""":- use_module(library(lists)).
:- consult('{src_posix}').
:- initialization(main, main).
main :-
    Input = {pl_list},
    statistics(walltime, [_, _]),
    (resolver(Input, _) -> true ; true),
    statistics(walltime, [_, T]),
    format('~w~n', [T]),
    halt.
"""

def _make_prolog_script_clp(puzzle: str, src: Path) -> str:
    """Script Prolog completo para el solver CLP(FD)."""
    matrix = puzzle_to_prolog_matrix(puzzle)
    src_posix = src.as_posix()
    return f""":- use_module(library(clpfd)).
:- consult('{src_posix}').
:- initialization(main, main).
main :-
    Matrix = {matrix},
    statistics(walltime, [_, _]),
    sudoku(Matrix),
    statistics(walltime, [_, T]),
    format('~w~n', [T]),
    halt.
"""

def _run_prolog_script(script_content: str) -> Dict:
    """
    Escribe el script en un archivo temporal y ejecuta swipl.
    Más robusto que -g en Windows: sin límite de longitud ni problemas
    con rutas que contienen espacios o backslashes.
    """
    tmp = None
    try:
        # Crear archivo temporal con extensión .pl
        with tempfile.NamedTemporaryFile(
            mode='w', suffix='.pl', delete=False,
            encoding='utf-8', newline='\n'
        ) as f:
            f.write(script_content)
            tmp = Path(f.name)

        ok_flag, stdout, stderr, timed_out = run_process(
            ['swipl', '-q', str(tmp)], TIMEOUT
        )

        if timed_out:
            return {"success": False, "timeout": True, "error": "Timeout"}
        if ok_flag and stdout:
            try:
                return {"success": True, "timeout": False, "time_ms": float(stdout.strip())}
            except ValueError:
                return {"success": False, "timeout": False,
                        "error": f"Bad output: {stdout!r}"}
        return {"success": False, "timeout": False,
                "error": (stderr[:120] if stderr else "No output")}
    finally:
        if tmp and tmp.exists():
            try:
                tmp.unlink()
            except Exception:
                pass

def benchmark_prolog(puzzle: str, solver_type: str) -> Dict:
    """
    Ejecuta RUNS_PER_PUZZLE runs del solver Prolog indicado.
    Retorna mediana de tiempos o dict de error.

    solver_type: "naive" | "mrv" | "clp"
    """
    script_makers = {
        "naive": (_make_prolog_script_naive, PROLOG_SRC / "sudoku_manual.pl"),
        "mrv":   (_make_prolog_script_mrv,   PROLOG_SRC / "sudoku_manual_mrv.pl"),
        "clp":   (_make_prolog_script_clp,   PROLOG_SRC / "sudoku_clp.pl"),
    }
    if solver_type not in script_makers:
        return {"success": False, "error": f"Unknown solver: {solver_type}"}

    make_script, src = script_makers[solver_type]

    if not src.exists():
        return {"success": False, "error": f"File not found: {src}"}

    times = []
    last_err = None

    # Warm-up: una ejecución descartada para que el SO cachee el ejecutable
    _run_prolog_script(make_script(puzzle, src))

    # Mediciones reales
    for _ in range(RUNS_PER_PUZZLE):
        r = _run_prolog_script(make_script(puzzle, src))
        if r["success"]:
            times.append(r["time_ms"])
        elif r.get("timeout"):
            return {"success": False, "timeout": True, "error": "Timeout", "all_times": []}
        else:
            last_err = r.get("error", "Unknown")

    if not times:
        return {"success": False, "timeout": False,
                "error": last_err or "No successful runs", "all_times": []}

    return {
        "success":   True,
        "timeout":   False,
        "time_ms":   statistics.median(times),
        "all_times": times,
        "error":     None,
    }

# ═══════════════════════════════════════════════════════════════════════
# RUNNERS HASKELL
# ═══════════════════════════════════════════════════════════════════════

# Mapa estrategia → argumento CLI del ejecutable Haskell
_HASKELL_STRATS = {
    "first":       "fe",
    "mrv":         "mrv",
    "propagation": "pmrv",
}

# Mapa dificultad → argumento CLI
_HASKELL_LEVELS = {
    "easy":   "easy",
    "medium": "medium",
    "hard":   "hard",
}

def _find_haskell_exe() -> Optional[Path]:
    """
    Busca el ejecutable Haskell compilado.
    Busca en rutas de Stack, luego en PATH (compatible Windows/Linux/macOS).
    """
    # 1. Buscar en .stack-work (nombres con y sin .exe para Windows)
    candidates = [
        HASKELL_DIR / ".stack-work",
        HASKELL_DIR / "dist-newstyle",
    ]
    for base in candidates:
        if not base.exists():
            continue
        for pattern in ("sudoku-exe.exe", "sudoku-exe"):
            for exe in base.rglob(pattern):
                if exe.is_file():
                    return exe

    # 2. Fallback: buscar en PATH del sistema
    which_cmd = "where" if sys.platform == "win32" else "which"
    ok_flag, stdout, _, _ = run_process([which_cmd, "sudoku-exe"], 5)
    if ok_flag and stdout:
        return Path(stdout.strip().splitlines()[0])  # where puede devolver varias líneas

    # 3. Ruta directa típica de stack install en Windows
    appdata = os.environ.get("APPDATA", "")
    if appdata:
        candidate = Path(appdata) / "local" / "bin" / "sudoku-exe.exe"
        if candidate.exists():
            return candidate

    return None

def benchmark_haskell(puzzle: str, strategy: str, difficulty: str) -> Dict:
    """
    Ejecuta RUNS_PER_PUZZLE runs del solver Haskell.
    Usa el modo --bench-external que imprime SOLO el tiempo en segundos.

    strategy:   "first" | "mrv" | "propagation"
    difficulty: "easy"  | "medium" | "hard"
    """
    if strategy not in _HASKELL_STRATS:
        return {"success": False, "error": f"Unknown strategy: {strategy}"}

    exe = _find_haskell_exe()
    if exe is None:
        return {
            "success": False,
            "error": (
                "Ejecutable Haskell no encontrado. "
                "Compilá con: cd haskell && stack build"
            )
        }

    strat_arg = _HASKELL_STRATS[strategy]
    level_arg = _HASKELL_LEVELS.get(difficulty, "easy")

    times = []
    last_err = None

    # Warm-up
    for _ in range(WARMUP_RUNS):
        run_process([str(exe), "--bench-external", strat_arg, level_arg], TIMEOUT)

    # Mediciones reales
    for _ in range(RUNS_PER_PUZZLE):
        ok_flag, stdout, stderr, timed_out = run_process(
            [str(exe), "--bench-external", strat_arg, level_arg], TIMEOUT
        )
        if timed_out:
            return {"success": False, "timeout": True, "error": "Timeout", "all_times": []}
        if ok_flag and stdout:
            try:
                t_sec = float(stdout)
                times.append(t_sec * 1000)   # convertir a ms
            except ValueError:
                last_err = f"Bad output: {stdout!r}"
        else:
            last_err = stderr[:120] or "No output"

    if not times:
        return {"success": False, "timeout": False, "error": last_err or "No runs", "all_times": []}

    return {
        "success":   True,
        "timeout":   False,
        "time_ms":   statistics.median(times),
        "all_times": times,
        "error":     None,
    }

# ═══════════════════════════════════════════════════════════════════════
# ORQUESTADOR PRINCIPAL
# ═══════════════════════════════════════════════════════════════════════

# Definición de todos los solvers
SOLVERS = [
    # (id_interno,       nombre_display,           runner_fn)
    ("prolog_naive",  "Prolog Naive BT",     lambda p, d: benchmark_prolog(p, "naive")),
    ("prolog_mrv",    "Prolog MRV",          lambda p, d: benchmark_prolog(p, "mrv")),
    ("prolog_clp",    "Prolog CLP(FD)",      lambda p, d: benchmark_prolog(p, "clp")),
    ("haskell_first", "Haskell FirstEmpty",  lambda p, d: benchmark_haskell(p, "first",       d)),
    ("haskell_mrv",   "Haskell MRV",         lambda p, d: benchmark_haskell(p, "mrv",         d)),
    ("haskell_prop",  "Haskell PropMRV",     lambda p, d: benchmark_haskell(p, "propagation", d)),
]

def run_full_benchmark(
    difficulties: List[str],
    solvers_filter: Optional[List[str]] = None,
    max_puzzles: Optional[int] = None,
) -> List[BenchmarkResult]:
    """
    Ejecuta el benchmark completo.

    Args:
        difficulties:   Lista de dificultades a probar.
        solvers_filter: Si se especifica, solo ejecuta esos solvers (por id).
        max_puzzles:    Límite de puzzles por dificultad (None = todos).
    """
    ph("BENCHMARK COMPARATIVO - SUDOKU SOLVERS")

    active_solvers = [
        (sid, name, fn) for sid, name, fn in SOLVERS
        if solvers_filter is None or sid in solvers_filter
    ]

    all_results: List[BenchmarkResult] = []

    for difficulty in difficulties:
        print(f"\n{C.BOLD}{'─'*70}\n  DIFICULTAD: {difficulty.upper()}\n{'─'*70}{C.ENDC}\n")

        puzzles = load_puzzles(difficulty)
        if not puzzles:
            err(f"Sin puzzles para '{difficulty}' — saltando")
            continue

        if max_puzzles:
            puzzles = puzzles[:max_puzzles]

        for puzzle_num, puzzle in enumerate(puzzles, 1):
            print(f"  {C.CYAN}Puzzle {puzzle_num:>3}/{len(puzzles)}{C.ENDC}")

            for solver_id, solver_name, runner in active_solvers:
                print(f"    {solver_name:<26}", end="", flush=True)

                r = runner(puzzle, difficulty)

                if r.get("success"):
                    t = r["time_ms"]
                    print(f"{C.GREEN}{t:>10.2f} ms  ✓{C.ENDC}")
                    all_results.append(BenchmarkResult(
                        solver=solver_id, difficulty=difficulty,
                        puzzle_num=puzzle_num, puzzle=puzzle,
                        time_ms=t, success=True, timeout=False,
                        all_times=r.get("all_times", []),
                    ))
                elif r.get("timeout"):
                    print(f"{C.YELLOW}{'TIMEOUT':>14}{C.ENDC}")
                    all_results.append(BenchmarkResult(
                        solver=solver_id, difficulty=difficulty,
                        puzzle_num=puzzle_num, puzzle=puzzle,
                        time_ms=TIMEOUT * 1000, success=False, timeout=True,
                        error="Timeout",
                    ))
                else:
                    msg = (r.get("error") or "Error desconocido")[:35]
                    print(f"{C.RED}{'ERROR: '+msg:>14}{C.ENDC}")
                    all_results.append(BenchmarkResult(
                        solver=solver_id, difficulty=difficulty,
                        puzzle_num=puzzle_num, puzzle=puzzle,
                        time_ms=0, success=False, timeout=False,
                        error=msg,
                    ))
            print()

    return all_results

# ═══════════════════════════════════════════════════════════════════════
# ANÁLISIS ESTADÍSTICO
# ═══════════════════════════════════════════════════════════════════════

def aggregate(results: List[BenchmarkResult]) -> List[AggregatedStats]:
    """Calcula estadísticas agregadas por (solver, difficulty)."""
    groups: Dict[tuple, List[float]] = {}
    timeouts: Dict[tuple, int] = {}
    totals:   Dict[tuple, int] = {}

    for r in results:
        key = (r.solver, r.difficulty)
        totals[key] = totals.get(key, 0) + 1
        if r.timeout:
            timeouts[key] = timeouts.get(key, 0) + 1
        if r.success and not r.timeout:
            groups.setdefault(key, []).append(r.time_ms)

    stats = []
    for key, times in groups.items():
        solver, diff = key
        n = len(times)
        stats.append(AggregatedStats(
            solver=solver, difficulty=diff,
            count=n,
            mean_ms=statistics.mean(times),
            median_ms=statistics.median(times),
            min_ms=min(times), max_ms=max(times),
            stddev_ms=statistics.stdev(times) if n > 1 else 0.0,
            success_rate=n / totals[key],
            timeout_count=timeouts.get(key, 0),
        ))
    return stats

def print_stats(stats: List[AggregatedStats]):
    ph("ESTADÍSTICAS AGREGADAS")
    for diff in ["easy", "medium", "hard"]:
        ds = sorted([s for s in stats if s.difficulty == diff], key=lambda x: x.median_ms)
        if not ds:
            continue
        print(f"\n{C.BOLD}{C.CYAN}  {'─'*68}\n  {diff.upper()}\n  {'─'*68}{C.ENDC}\n")
        header = f"  {'Solver':<22} {'N':>4}  {'Median':>10}  {'Mean':>10}  {'Min':>9}  {'Max':>10}  {'σ':>9}  {'OK%':>5}"
        print(f"{C.DIM}{header}{C.ENDC}")
        print(f"  {'─'*22}  {'─'*4}  {'─'*10}  {'─'*10}  {'─'*9}  {'─'*10}  {'─'*9}  {'─'*5}")
        for s in ds:
            ok_pct = f"{s.success_rate*100:.0f}%"
            print(
                f"  {s.solver:<22} {s.count:>4}  "
                f"{s.median_ms:>9.1f}ms  {s.mean_ms:>9.1f}ms  "
                f"{s.min_ms:>8.1f}ms  {s.max_ms:>9.1f}ms  "
                f"{s.stddev_ms:>8.1f}ms  {ok_pct:>5}"
            )

def print_speedups(stats: List[AggregatedStats]):
    """Imprime tabla de speedups entre pares de solvers."""
    ph("FACTORES DE SPEEDUP (mediana)")

    comparisons = [
        ("prolog_naive",  "prolog_mrv",    "Prolog Naive   → Prolog MRV"),
        ("prolog_mrv",    "prolog_clp",    "Prolog MRV     → Prolog CLP(FD)"),
        ("prolog_naive",  "prolog_clp",    "Prolog Naive   → Prolog CLP(FD)"),
        ("haskell_first", "haskell_mrv",   "Haskell First  → Haskell MRV"),
        ("haskell_mrv",   "haskell_prop",  "Haskell MRV    → Haskell PropMRV"),
        ("prolog_mrv",    "haskell_mrv",   "Prolog MRV     → Haskell MRV"),
        ("prolog_clp",    "haskell_prop",  "Prolog CLP(FD) → Haskell PropMRV"),
    ]

    for diff in ["easy", "medium", "hard"]:
        ds = {s.solver: s.median_ms for s in stats if s.difficulty == diff}
        if not ds:
            continue
        print(f"\n{C.BOLD}  {diff.upper()}{C.ENDC}")
        for slow, fast, label in comparisons:
            if slow in ds and fast in ds and ds[fast] > 0 and ds[slow] > 0:
                factor = ds[slow] / ds[fast]
                direction = "más rápido" if factor >= 1 else "más lento"
                f_abs = factor if factor >= 1 else 1 / factor
                bar = "█" * min(int(f_abs), 40)
                print(f"    {label:<40}  {f_abs:>6.1f}× {direction}  {C.CYAN}{bar}{C.ENDC}")
            elif slow in ds and fast in ds:
                print(f"    {label:<40}  {C.YELLOW}N/A — mediana 0 ms (puzzles triviales en muestra){C.ENDC}")

# ═══════════════════════════════════════════════════════════════════════
# GUARDADO DE RESULTADOS
# ═══════════════════════════════════════════════════════════════════════

def save_results(results: List[BenchmarkResult], stats: List[AggregatedStats]):
    """Exporta resultados en CSV y JSON."""
    # CSV detallado
    csv_path = RESULTS_DIR / "benchmark_results.csv"
    with open(csv_path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=[
            "solver", "difficulty", "puzzle_num", "puzzle",
            "time_ms", "success", "timeout", "error",
        ])
        w.writeheader()
        for r in results:
            row = asdict(r)
            row.pop("all_times", None)  # no serializar lista anidada en CSV plano
            w.writerow(row)
    ok(f"CSV detallado: {csv_path}")

    # CSV estadísticas
    stats_path = RESULTS_DIR / "statistics.csv"
    with open(stats_path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(AggregatedStats.__dataclass_fields__.keys()))
        w.writeheader()
        w.writerows([asdict(s) for s in stats])
    ok(f"CSV estadísticas: {stats_path}")

    # JSON completo
    json_path = RESULTS_DIR / "results.json"
    with open(json_path, "w") as f:
        json.dump({
            "metadata": {
                "runs_per_puzzle": RUNS_PER_PUZZLE,
                "warmup_runs":     WARMUP_RUNS,
                "timeout_s":       TIMEOUT,
                "timestamp":       time.strftime("%Y-%m-%dT%H:%M:%S"),
            },
            "results":    [asdict(r) for r in results],
            "statistics": [asdict(s) for s in stats],
        }, f, indent=2)
    ok(f"JSON: {json_path}")

# ═══════════════════════════════════════════════════════════════════════
# VISUALIZACIÓN
# ═══════════════════════════════════════════════════════════════════════

# Etiquetas de display para cada solver
SOLVER_LABELS = {
    "prolog_naive":  "Prolog\nNaive BT",
    "prolog_mrv":    "Prolog\nMRV",
    "prolog_clp":    "Prolog\nCLP(FD)",
    "haskell_first": "Haskell\nFirstEmpty",
    "haskell_mrv":   "Haskell\nMRV",
    "haskell_prop":  "Haskell\nPropMRV",
}

# Colores por paradigma
SOLVER_COLORS = {
    "prolog_naive":  "#E74C3C",
    "prolog_mrv":    "#E67E22",
    "prolog_clp":    "#27AE60",
    "haskell_first": "#2980B9",
    "haskell_mrv":   "#8E44AD",
    "haskell_prop":  "#16A085",
}

def generate_plots(stats: List[AggregatedStats], results: List[BenchmarkResult]):
    """
    Genera 4 gráficos:
      1. Barras agrupadas: tiempos medianos por solver y dificultad
      2. Escala log: idem en escala logarítmica
      3. Box plot: distribución de tiempos por solver
      4. Heatmap: speedup relativo entre solvers
    """
    try:
        import matplotlib
        matplotlib.use("Agg")          # sin display X
        import matplotlib.pyplot as plt
        import matplotlib.patches as mpatches
        import numpy as np
    except ImportError:
        warn("matplotlib no disponible. Instalar con: pip install matplotlib")
        return

    DIFFICULTIES = ["easy", "medium", "hard"]
    SOLVER_ORDER = [s[0] for s in SOLVERS]
    N = len(SOLVER_ORDER)

    # ── Figura 1: Barras agrupadas (escala lineal) ──────────────────────
    fig, axes = plt.subplots(1, 3, figsize=(18, 6))
    fig.suptitle("Tiempo mediano de resolución por solver y dificultad", fontsize=14, fontweight="bold")

    for ax, diff in zip(axes, DIFFICULTIES):
        ds = {s.solver: s.median_ms for s in stats if s.difficulty == diff}
        x  = np.arange(N)
        vals  = [ds.get(sid, 0) for sid in SOLVER_ORDER]
        colors = [SOLVER_COLORS[sid] for sid in SOLVER_ORDER]
        bars  = ax.bar(x, vals, color=colors, width=0.6, edgecolor="white", linewidth=0.8)
        ax.set_xticks(x)
        ax.set_xticklabels([SOLVER_LABELS[s] for s in SOLVER_ORDER], fontsize=7)
        ax.set_ylabel("Tiempo (ms)")
        ax.set_title(diff.capitalize())
        ax.grid(axis="y", alpha=0.3)
        for bar, v in zip(bars, vals):
            if v > 0:
                ax.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.5,
                        f"{v:.1f}", ha="center", va="bottom", fontsize=6.5)

    # Leyenda paradigmas
    legend_handles = [
        mpatches.Patch(color="#E74C3C", label="Prolog"),
        mpatches.Patch(color="#2980B9", label="Haskell"),
    ]
    fig.legend(handles=legend_handles, loc="lower center", ncol=2, fontsize=9,
               bbox_to_anchor=(0.5, -0.02))
    plt.tight_layout(rect=[0, 0.04, 1, 1])
    out = RESULTS_DIR / "fig1_bar_linear.png"
    plt.savefig(out, dpi=150, bbox_inches="tight")
    plt.close()
    ok(f"Gráfico 1 → {out}")

    # ── Figura 2: Barras agrupadas (escala log) ─────────────────────────
    fig, axes = plt.subplots(1, 3, figsize=(18, 6))
    fig.suptitle("Tiempo mediano — escala logarítmica", fontsize=14, fontweight="bold")

    for ax, diff in zip(axes, DIFFICULTIES):
        ds   = {s.solver: s.median_ms for s in stats if s.difficulty == diff}
        x    = np.arange(N)
        vals = [max(ds.get(sid, 0.01), 0.01) for sid in SOLVER_ORDER]
        colors = [SOLVER_COLORS[sid] for sid in SOLVER_ORDER]
        ax.bar(x, vals, color=colors, width=0.6, edgecolor="white", linewidth=0.8)
        ax.set_yscale("log")
        ax.set_xticks(x)
        ax.set_xticklabels([SOLVER_LABELS[s] for s in SOLVER_ORDER], fontsize=7)
        ax.set_ylabel("Tiempo (ms) — log")
        ax.set_title(diff.capitalize())
        ax.grid(axis="y", alpha=0.3, which="both")

    fig.legend(handles=legend_handles, loc="lower center", ncol=2, fontsize=9,
               bbox_to_anchor=(0.5, -0.02))
    plt.tight_layout(rect=[0, 0.04, 1, 1])
    out = RESULTS_DIR / "fig2_bar_log.png"
    plt.savefig(out, dpi=150, bbox_inches="tight")
    plt.close()
    ok(f"Gráfico 2 → {out}")

    # ── Figura 3: Box plot de distribuciones ────────────────────────────
    # Datos raw por solver (todas las dificultades combinadas)
    raw: Dict[str, List[float]] = {sid: [] for sid in SOLVER_ORDER}
    for r in results:
        if r.success and not r.timeout:
            raw[r.solver].extend(r.all_times if r.all_times else [r.time_ms])

    fig, ax = plt.subplots(figsize=(12, 6))
    data    = [raw[sid] for sid in SOLVER_ORDER]
    labels  = [SOLVER_LABELS[sid].replace("\n", " ") for sid in SOLVER_ORDER]
    colors  = [SOLVER_COLORS[sid] for sid in SOLVER_ORDER]

    bp = ax.boxplot(data, tick_labels=labels, patch_artist=True,
                    medianprops=dict(color="black", linewidth=2))
    for patch, color in zip(bp["boxes"], colors):
        patch.set_facecolor(color)
        patch.set_alpha(0.7)

    ax.set_yscale("log")
    ax.set_ylabel("Tiempo (ms) — log")
    ax.set_title("Distribución de tiempos de resolución (todas las dificultades)")
    ax.grid(axis="y", alpha=0.3, which="both")
    plt.xticks(rotation=15)
    plt.tight_layout()
    out = RESULTS_DIR / "fig3_boxplot.png"
    plt.savefig(out, dpi=150, bbox_inches="tight")
    plt.close()
    ok(f"Gráfico 3 → {out}")

    # ── Figura 4: Heatmap speedup (pares, dificultad medium) ────────────
    diff_stats = {s.solver: s.median_ms for s in stats if s.difficulty == "medium"}
    if len(diff_stats) >= 2:
        matrix = np.zeros((N, N))
        for i, si in enumerate(SOLVER_ORDER):
            for j, sj in enumerate(SOLVER_ORDER):
                ti = diff_stats.get(si, 0)
                tj = diff_stats.get(sj, 0)
                if ti > 0 and tj > 0:
                    matrix[i, j] = ti / tj

        fig, ax = plt.subplots(figsize=(9, 7))
        im = ax.imshow(matrix, cmap="RdYlGn", aspect="auto",
                       norm=matplotlib.colors.LogNorm(vmin=0.1, vmax=max(matrix.max(), 1.1)))
        cbar = fig.colorbar(im, ax=ax)
        cbar.set_label("Speedup (fila/columna)")

        short = [SOLVER_LABELS[s].replace("\n", " ") for s in SOLVER_ORDER]
        ax.set_xticks(range(N)); ax.set_xticklabels(short, rotation=30, ha="right", fontsize=8)
        ax.set_yticks(range(N)); ax.set_yticklabels(short, fontsize=8)
        ax.set_title("Speedup relativo — dificultad Media\n(valor > 1 = fila más rápida que columna)")

        for i in range(N):
            for j in range(N):
                v = matrix[i, j]
                txt = f"{v:.1f}×" if v > 0 else "—"
                ax.text(j, i, txt, ha="center", va="center", fontsize=7,
                        color="black" if 0.4 < v < 4 else "white")

        plt.tight_layout()
        out = RESULTS_DIR / "fig4_heatmap_speedup.png"
        plt.savefig(out, dpi=150, bbox_inches="tight")
        plt.close()
        ok(f"Gráfico 4 → {out}")

# ═══════════════════════════════════════════════════════════════════════
# VERIFICACIÓN DEL ENTORNO
# ═══════════════════════════════════════════════════════════════════════

def check_environment() -> bool:
    """Verifica que las herramientas necesarias estén disponibles."""
    ph("VERIFICANDO ENTORNO")
    all_ok = True

    # SWI-Prolog
    ok_flag, stdout, _, _ = run_process(["swipl", "--version"], 10)
    if ok_flag:
        ok(f"SWI-Prolog: {stdout.splitlines()[0]}")
    else:
        err("SWI-Prolog no encontrado (instalar con: sudo apt install swi-prolog)")
        all_ok = False

    # Archivos Prolog
    for name in ["sudoku_manual.pl", "sudoku_manual_mrv.pl", "sudoku_clp.pl"]:
        p = PROLOG_SRC / name
        if p.exists():
            ok(f"Prolog source: {name}")
        else:
            err(f"Prolog source FALTANTE: {p}")
            all_ok = False

    # Ejecutable Haskell
    exe = _find_haskell_exe()
    if exe:
        ok(f"Haskell exe: {exe}")
    else:
        warn("Ejecutable Haskell no encontrado (compilar con: cd haskell && stack build)")

    # Puzzles
    for diff in ["easy", "medium", "hard"]:
        p = PUZZLES_DIR / diff / "puzzles.txt"
        if p.exists():
            with open(p) as f:
                n = sum(1 for l in f if len(l.strip()) == 81)
            ok(f"Puzzles {diff}: {n} cargados")
        else:
            err(f"Puzzles FALTANTES: {p}")
            all_ok = False

    # matplotlib
    try:
        import matplotlib
        ok(f"matplotlib {matplotlib.__version__}")
    except ImportError:
        warn("matplotlib no instalado (gráficos deshabilitados — pip install matplotlib)")

    return all_ok

# ═══════════════════════════════════════════════════════════════════════
# CLI
# ═══════════════════════════════════════════════════════════════════════

def main():
    global RUNS_PER_PUZZLE

    parser = argparse.ArgumentParser(
        description="Benchmark comparativo: Prolog vs Haskell — Sudoku Solvers",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Ejemplos de uso:
  python comparison.py                             # Benchmark completo
  python comparison.py --difficulty easy           # Solo dificultad fácil
  python comparison.py --difficulty easy medium    # Dos dificultades
  python comparison.py --solvers prolog_clp haskell_prop  # Solo esos solvers
  python comparison.py --quick                     # 1 puzzle por dificultad
  python comparison.py --check                     # Verificar entorno
  python comparison.py --plot                      # Incluir gráficos
        """,
    )
    parser.add_argument(
        "--difficulty", nargs="+",
        choices=["easy", "medium", "hard"],
        default=["easy", "medium", "hard"],
        metavar="LEVEL",
        help="Niveles de dificultad a probar (default: todos)",
    )
    parser.add_argument(
        "--solvers", nargs="+",
        choices=[s[0] for s in SOLVERS],
        default=None,
        metavar="SOLVER_ID",
        help="Solvers específicos a ejecutar (default: todos)",
    )
    parser.add_argument(
        "--quick", action="store_true",
        help="Modo rápido: 1 puzzle por dificultad, 1 run (para testing)",
    )
    parser.add_argument(
        "--plot", action="store_true",
        help="Generar gráficos PNG (requiere matplotlib)",
    )
    parser.add_argument(
        "--check", action="store_true",
        help="Solo verificar entorno y salir",
    )
    parser.add_argument(
        "--runs", type=int, default=RUNS_PER_PUZZLE,
        help=f"Ejecuciones por solver×puzzle (default: {RUNS_PER_PUZZLE})",
    )

    args = parser.parse_args()

    # Verificar entorno
    env_ok = check_environment()
    if args.check:
        sys.exit(0 if env_ok else 1)

    # Ajustar parámetros
    RUNS_PER_PUZZLE = args.runs
    if args.quick:
        RUNS_PER_PUZZLE = 1
    max_puzzles = 1 if args.quick else None

    if not env_ok:
        warn("Hay problemas en el entorno. Continuar de todas formas...")

    # Ejecutar benchmark
    results = run_full_benchmark(
        difficulties=args.difficulty,
        solvers_filter=args.solvers,
        max_puzzles=max_puzzles,
    )

    if not results:
        err("Sin resultados. ¿Están los puzzles y solvers disponibles?")
        sys.exit(1)

    # Estadísticas
    stats = aggregate(results)
    print_stats(stats)
    print_speedups(stats)

    # Guardar
    save_results(results, stats)

    # Gráficos
    if args.plot:
        generate_plots(stats, results)

    ph("BENCHMARK COMPLETADO")
    ok(f"Resultados en: {RESULTS_DIR}")

if __name__ == "__main__":
    main()