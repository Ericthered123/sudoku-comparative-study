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
  - Warm-up (WARMUP_RUNS ejecuciones) descartado antes de medir
  - Métrica: tiempo de alta resolución
      · Prolog:  get_time/1 → Unix timestamp float (QueryPerformanceCounter
                 en Windows, ~100 ns resolución) → diferencia × 1000 → ms.
                 statistics(cputime/walltime) descartado: ambos tienen
                 granularidad de 15.625 ms en Windows (GetProcessTimes, 64 Hz)
                 produciendo ceros y múltiplos exactos inutilizables.
      · Haskell: getCPUTime (System.CPUTime) → picosegundos → ×1e-9 → ms.
                 Usa QueryPerformanceCounter internamente → sub-ms real.
      · Para un solver single-threaded sin I/O, wall time de alta resolución
        ≈ CPU time: la diferencia es despreciable en la práctica.
  - Puzzles en formato de 81 caracteres (0/. = celda vacía)
  - Resultados exportados en CSV, JSON y PNG
"""

import re
import subprocess
import time
import csv
import os
import sys
import json
import statistics
import argparse
import tempfile
from pathlib import Path
from dataclasses import dataclass, asdict, field
from typing import List, Dict, Optional

# ═══════════════════════════════════════════════════════════════════════
# RUTAS DEL PROYECTO
# ═══════════════════════════════════════════════════════════════════════

PROJECT_ROOT = Path(__file__).parent.parent
PUZZLES_DIR  = PROJECT_ROOT / "benchmarks" / "puzzles"
RESULTS_DIR  = PROJECT_ROOT / "benchmarks" / "results"
PROLOG_SRC   = PROJECT_ROOT / "prolog" / "src"
HASKELL_DIR  = PROJECT_ROOT / "haskell"

RESULTS_DIR.mkdir(parents=True, exist_ok=True)

# ═══════════════════════════════════════════════════════════════════════
# PARÁMETROS GLOBALES
# ═══════════════════════════════════════════════════════════════════════

TIMEOUT         = 30    # segundos por ejecución individual
RUNS_PER_PUZZLE = 5     # ejecuciones por solver×puzzle (se toma la mediana)
WARMUP_RUNS     = 1     # ejecuciones de calentamiento descartadas

# ═══════════════════════════════════════════════════════════════════════
# COLORES ANSI
# ═══════════════════════════════════════════════════════════════════════

class C:
    HEADER = '\033[95m'; CYAN   = '\033[96m'; GREEN  = '\033[92m'
    YELLOW = '\033[93m'; RED    = '\033[91m'; ENDC   = '\033[0m'
    BOLD   = '\033[1m';  DIM    = '\033[2m'

def ph(t):   print(f"\n{C.HEADER}{'═'*70}\n{C.BOLD}{t:^70}{C.ENDC}{C.HEADER}\n{'═'*70}{C.ENDC}\n")
def ok(t):   print(f"{C.GREEN}✓ {t}{C.ENDC}")
def err(t):  print(f"{C.RED}✗ {t}{C.ENDC}")
def warn(t): print(f"{C.YELLOW}⚠ {t}{C.ENDC}")
def info(t): print(f"{C.CYAN}→ {t}{C.ENDC}")

# ═══════════════════════════════════════════════════════════════════════
# ESTRUCTURAS DE DATOS
# ═══════════════════════════════════════════════════════════════════════

@dataclass
class BenchmarkResult:
    solver:     str
    difficulty: str
    puzzle_num: int
    puzzle:     str
    time_ms:    float
    success:    bool
    timeout:    bool
    error:      Optional[str] = None
    all_times:  List[float]   = field(default_factory=list)

@dataclass
class AggregatedStats:
    solver:        str
    difficulty:    str
    count:         int
    mean_ms:       float
    median_ms:     float
    min_ms:        float
    max_ms:        float
    stddev_ms:     float
    success_rate:  float
    timeout_count: int

# ═══════════════════════════════════════════════════════════════════════
# CARGA DE PUZZLES
# ═══════════════════════════════════════════════════════════════════════

def load_puzzles(difficulty: str) -> List[str]:
    """Carga puzzles desde archivo; cada línea válida tiene exactamente 81 chars."""
    path = PUZZLES_DIR / difficulty / "puzzles.txt"
    if not path.exists():
        err(f"Archivo no encontrado: {path}")
        return []
    with open(path) as f:
        puzzles = [l.strip() for l in f if len(l.strip()) == 81]
    info(f"Cargados {len(puzzles)} puzzles [{difficulty}]")
    return puzzles

# ═══════════════════════════════════════════════════════════════════════
# CONVERSIÓN DE PUZZLES
# ═══════════════════════════════════════════════════════════════════════

def puzzle_to_prolog_list(puzzle: str) -> str:
    """
    Lista Prolog con átomo 'x' para celdas vacías.
    Usada por los solvers Naive BT y MRV, que esperan 'x' como marcador.
    Ejemplo: "530070000..." → "[5,3,x,x,7,x,x,x,x,...]"
    """
    tokens = ['x' if c in '0.' else c for c in puzzle]
    return '[' + ','.join(tokens) + ']'

def puzzle_to_flat_list(puzzle: str) -> str:
    """
    Lista Prolog de enteros con 0 para celdas vacías.
    Usada por CLP(FD): el script Prolog convierte 0 → variable fresca.
    Ejemplo: "530070000..." → "[5,3,0,0,7,0,0,0,0,...]"
    """
    tokens = ['0' if c in '0.' else c for c in puzzle]
    return '[' + ','.join(tokens) + ']'

# ═══════════════════════════════════════════════════════════════════════
# EJECUCIÓN DE PROCESOS
# ═══════════════════════════════════════════════════════════════════════

def run_process(cmd: List[str], timeout: int) -> tuple:
    """
    Ejecuta proceso externo. Retorna (success, stdout, stderr, timed_out).

    En Windows, subprocess.run con timeout lanza TimeoutExpired pero deja
    el proceso hijo corriendo (no lo mata). Hay que usar Popen + kill()
    explícito para garantizar que el proceso termina y los pipes se vacían.
    """
    proc = subprocess.Popen(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
    )
    try:
        stdout, stderr = proc.communicate(timeout=timeout)
        return proc.returncode == 0, stdout.strip(), stderr.strip(), False
    except subprocess.TimeoutExpired:
        proc.kill()
        proc.communicate()   # vaciar pipes para evitar deadlock
        return False, "", "TIMEOUT", True
    except FileNotFoundError as e:
        return False, "", str(e), False

# ═══════════════════════════════════════════════════════════════════════
# SCRIPTS PROLOG
#
# Métrica: get_time/1  (SWI-Prolog built-in)
#   - Devuelve Unix timestamp como float de alta resolución
#   - Usa QueryPerformanceCounter en Windows → resolución ~100 ns
#   - statistics(cputime) y statistics(walltime) en Windows tienen
#     resolución de solo 15.625 ms (GetProcessTimes, timer a 64 Hz)
#     y producen múltiplos de ese valor, lo que da ceros para puzzles
#     rápidos y granularidad inaceptable para benchmarks.
#   - Para un proceso single-threaded sin I/O, wall time de alta
#     resolución ≈ CPU time: la diferencia es despreciable.
#   - Patrón: get_time(T1) antes, get_time(T2) después,
#             Elapsed is (T2 - T1) * 1000.0  →  ms con decimales reales
# ═══════════════════════════════════════════════════════════════════════

def _make_prolog_script_naive(puzzle: str, src: Path) -> str:
    """
    Script para el solver Naive BT. Entrada: lista con 'x' para vacíos.
    Usa get_time/1 para resolución sub-ms en Windows (QueryPerformanceCounter).
    """
    return f""":- use_module(library(lists)).
:- consult('{src.as_posix()}').
:- initialization(main, main).
main :-
    Input = {puzzle_to_prolog_list(puzzle)},
    get_time(T1),
    ( sudoku(Input, _) -> true ; true ),
    get_time(T2),
    Elapsed is (T2 - T1) * 1000.0,
    format('~6f~n', [Elapsed]),
    halt.
"""

def _make_prolog_script_mrv(puzzle: str, src: Path) -> str:
    """
    Script para el solver MRV. Entrada: lista con 'x' para vacíos.
    Usa get_time/1 para resolución sub-ms en Windows (QueryPerformanceCounter).
    """
    return f""":- use_module(library(lists)).
:- consult('{src.as_posix()}').
:- initialization(main, main).
main :-
    Input = {puzzle_to_prolog_list(puzzle)},
    get_time(T1),
    ( resolver(Input, _) -> true ; true ),
    get_time(T2),
    Elapsed is (T2 - T1) * 1000.0,
    format('~6f~n', [Elapsed]),
    halt.
"""

def _make_prolog_script_clp(puzzle: str, src: Path) -> str:
    """
    Script para el solver CLP(FD).

    El puzzle se pasa como lista plana de enteros (0 = vacío) para evitar
    el error aritmético que ocurre cuando se usan átomos como 'x' en
    expresiones del tipo N > 0.

    clues_to_vars/2: 0 → variable fresca anónima (_); N>0 → entero N.
    flat_to_rows/2:  lista plana de 81 → matriz 9×9 de listas.
    sudoku/1:        recibe la matriz con variables reales de Prolog.

    Usa get_time/1 para resolución sub-ms en Windows (QueryPerformanceCounter).
    """
    return f""":- use_module(library(clpfd)).
:- consult('{src.as_posix()}').
:- initialization(main, main).

clues_to_vars([], []).
clues_to_vars([0|Cs], [_|Vs]) :- !, clues_to_vars(Cs, Vs).
clues_to_vars([N|Cs], [N|Vs]) :- integer(N), N > 0, !, clues_to_vars(Cs, Vs).

flat_to_rows([], []).
flat_to_rows(Flat, [Row|Rows]) :-
    length(Row, 9), append(Row, Rest, Flat), flat_to_rows(Rest, Rows).

main :-
    Clues = {puzzle_to_flat_list(puzzle)},
    clues_to_vars(Clues, Vars),
    flat_to_rows(Vars, Matrix),
    get_time(T1),
    ( sudoku(Matrix) -> true ; true ),
    get_time(T2),
    Elapsed is (T2 - T1) * 1000.0,
    format('~6f~n', [Elapsed]),
    halt.
"""

# ─── Ejecución de scripts temporales ────────────────────────────────────

def _run_prolog_script(script_content: str) -> Dict:
    """
    Escribe el script en un archivo .pl temporal, lo ejecuta con swipl
    y parsea el tiempo CPU en ms del stdout.
    Warnings en stderr son ignorados si stdout contiene un número válido.
    """
    tmp = None
    try:
        RESULTS_DIR.mkdir(parents=True, exist_ok=True)
        with tempfile.NamedTemporaryFile(
            mode='w', suffix='.pl', delete=False,
            encoding='utf-8', newline='\n', dir=RESULTS_DIR
        ) as f:
            f.write(script_content)
            tmp = Path(f.name)

        ok_flag, stdout, stderr, timed_out = run_process(
            ['swipl', '-q', tmp.as_posix()], TIMEOUT
        )

        if timed_out:
            return {"success": False, "timeout": True, "error": "Timeout"}

        if stdout:
            m = re.search(r'([0-9]+\.[0-9]+|[0-9]+)', stdout)
            if m:
                return {"success": True, "timeout": False, "time_ms": float(m.group(1))}

        # Sin número válido en stdout → debug y error
        print("  STDOUT:", repr(stdout))
        print("  STDERR:", repr(stderr[:300]))
        return {"success": False, "timeout": False,
                "error": stderr[:200] if stderr else "No numeric output"}

    finally:
        if tmp and tmp.exists():
            try:
                tmp.unlink()
            except Exception:
                pass

def benchmark_prolog(puzzle: str, solver_type: str) -> Dict:
    """
    Ejecuta RUNS_PER_PUZZLE mediciones CPU del solver Prolog indicado
    y retorna la mediana, o un dict de error.
    """
    script_makers = {
        "naive": (_make_prolog_script_naive, PROLOG_SRC / "sudoku_manual.pl"),
        "mrv":   (_make_prolog_script_mrv,   PROLOG_SRC / "sudoku_manual_mrv.pl"),
        "clp":   (_make_prolog_script_clp,   PROLOG_SRC / "sudoku_clp.pl"),
    }
    if solver_type not in script_makers:
        return {"success": False, "error": f"Solver desconocido: {solver_type}"}

    make_script, src = script_makers[solver_type]
    if not src.exists():
        return {"success": False, "error": f"Fuente no encontrada: {src}"}

    # Warm-up: descartado, sirve para cachear el ejecutable en el SO
    _run_prolog_script(make_script(puzzle, src))

    times = []
    last_err = None
    for _ in range(RUNS_PER_PUZZLE):
        r = _run_prolog_script(make_script(puzzle, src))
        if r["success"]:
            times.append(r["time_ms"])
        elif r.get("timeout"):
            return {"success": False, "timeout": True, "error": "Timeout", "all_times": []}
        else:
            last_err = r.get("error", "Error desconocido")

    if not times:
        return {"success": False, "timeout": False,
                "error": last_err or "Sin runs exitosos", "all_times": []}
    return {"success": True, "timeout": False,
            "time_ms": statistics.median(times), "all_times": times, "error": None}

# ═══════════════════════════════════════════════════════════════════════
# RUNNER HASKELL
#
# El ejecutable usa getCPUTime (System.CPUTime) e imprime segundos float.
# getCPUTime devuelve picosegundos (Integer); dividir por 1e12 → segundos.
# Python recibe ese float y multiplica por 1000 para obtener ms.
# ═══════════════════════════════════════════════════════════════════════

_HASKELL_STRATS = {"first": "fe", "mrv": "mrv", "propagation": "pmrv"}

def _find_haskell_exe() -> Optional[Path]:
    """Busca el ejecutable compilado: .stack-work → dist-newstyle → PATH → APPDATA."""
    for base in [HASKELL_DIR / ".stack-work", HASKELL_DIR / "dist-newstyle"]:
        if not base.exists():
            continue
        for pattern in ("sudoku-exe.exe", "sudoku-exe"):
            for exe in base.rglob(pattern):
                if exe.is_file():
                    return exe

    which_cmd = "where" if sys.platform == "win32" else "which"
    found, stdout, _, _ = run_process([which_cmd, "sudoku-exe"], 5)
    if found and stdout:
        return Path(stdout.strip().splitlines()[0])

    appdata = os.environ.get("APPDATA", "")
    if appdata:
        candidate = Path(appdata) / "local" / "bin" / "sudoku-exe.exe"
        if candidate.exists():
            return candidate

    return None

def benchmark_haskell(puzzle: str, strategy: str, difficulty: str) -> Dict:
    """
    Ejecuta RUNS_PER_PUZZLE mediciones CPU del solver Haskell.

    Pasa el puzzle real (cadena de 81 chars) como argumento posicional al
    ejecutable: --bench-external <strat> <puzzle81>
    El ejecutable imprime el tiempo en segundos (float); se convierte a ms.

    NOTA: el argumento `difficulty` ya no se usa para seleccionar el puzzle
    en Haskell — antes el ejecutable resolvía siempre el mismo ejemplo
    hardcodeado (exampleEasy/Medium/Hard), lo que hacía que los 20 puzzles
    del benchmark dieran tiempos idénticos. Ahora recibe el puzzle real.
    """
    if strategy not in _HASKELL_STRATS:
        return {"success": False, "error": f"Estrategia desconocida: {strategy}"}

    exe = _find_haskell_exe()
    if exe is None:
        return {"success": False,
                "error": "Ejecutable Haskell no encontrado. Compilá: cd haskell && stack build"}

    strat_arg = _HASKELL_STRATS[strategy]
    cmd = [str(exe), "--bench-external", strat_arg, puzzle]

    # Warm-up descartado
    run_process(cmd, TIMEOUT)

    times = []
    last_err = None
    for _ in range(RUNS_PER_PUZZLE):
        success, stdout, stderr, timed_out = run_process(cmd, TIMEOUT)

        if timed_out:
            return {"success": False, "timeout": True, "error": "Timeout", "all_times": []}

        if stdout:
            # Extraer primer número float del stdout (robusto ante basura extra)
            m = re.search(r'([0-9]+\.[0-9]+|[0-9]+)', stdout)
            if m:
                try:
                    times.append(float(m.group(1)) * 1000.0)
                    continue
                except ValueError:
                    pass

        last_err = (stderr[:120] if stderr else f"Salida inesperada: {stdout!r}"[:120])

    if not times:
        return {"success": False, "timeout": False,
                "error": last_err or "Sin runs exitosos", "all_times": []}
    return {"success": True, "timeout": False,
            "time_ms": statistics.median(times), "all_times": times, "error": None}

# ═══════════════════════════════════════════════════════════════════════
# DEFINICIÓN DE SOLVERS
# ═══════════════════════════════════════════════════════════════════════

SOLVERS = [
    ("prolog_naive",  "Prolog Naive BT",    lambda p, d: benchmark_prolog(p, "naive")),
    ("prolog_mrv",    "Prolog MRV",         lambda p, d: benchmark_prolog(p, "mrv")),
    ("prolog_clp",    "Prolog CLP(FD)",     lambda p, d: benchmark_prolog(p, "clp")),
    ("haskell_first", "Haskell FirstEmpty", lambda p, d: benchmark_haskell(p, "first",       d)),
    ("haskell_mrv",   "Haskell MRV",        lambda p, d: benchmark_haskell(p, "mrv",         d)),
    ("haskell_prop",  "Haskell PropMRV",    lambda p, d: benchmark_haskell(p, "propagation", d)),
]

# ═══════════════════════════════════════════════════════════════════════
# ORQUESTADOR PRINCIPAL
# ═══════════════════════════════════════════════════════════════════════

def run_full_benchmark(
    difficulties:   List[str],
    solvers_filter: Optional[List[str]] = None,
    max_puzzles:    Optional[int]        = None,
) -> List[BenchmarkResult]:
    """
    Ejecuta el benchmark completo iterando sobre dificultades y puzzles.
    Cada runner está envuelto en try/except para que un crash aislado
    no detenga el proceso completo.
    """
    ph("BENCHMARK COMPARATIVO - SUDOKU SOLVERS  [CPU time]")

    active = [(sid, name, fn) for sid, name, fn in SOLVERS
              if solvers_filter is None or sid in solvers_filter]

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

            for solver_id, solver_name, runner in active:
                print(f"    {solver_name:<26}", end="", flush=True)

                try:
                    r = runner(puzzle, difficulty)
                except Exception as e:
                    r = {"success": False, "timeout": False, "error": str(e)[:80]}

                if r.get("success"):
                    t = r["time_ms"]
                    print(f"{C.GREEN}{t:>10.4f} ms  ✓{C.ENDC}")
                    all_results.append(BenchmarkResult(
                        solver=solver_id, difficulty=difficulty,
                        puzzle_num=puzzle_num, puzzle=puzzle,
                        time_ms=t, success=True, timeout=False,
                        all_times=r.get("all_times", [])))

                elif r.get("timeout"):
                    print(f"{C.YELLOW}{'TIMEOUT':>14}{C.ENDC}")
                    all_results.append(BenchmarkResult(
                        solver=solver_id, difficulty=difficulty,
                        puzzle_num=puzzle_num, puzzle=puzzle,
                        time_ms=TIMEOUT * 1000, success=False, timeout=True,
                        error="Timeout"))

                else:
                    msg = (r.get("error") or "Error desconocido")[:35]
                    print(f"{C.RED}  ERROR: {msg}{C.ENDC}")
                    all_results.append(BenchmarkResult(
                        solver=solver_id, difficulty=difficulty,
                        puzzle_num=puzzle_num, puzzle=puzzle,
                        time_ms=0, success=False, timeout=False, error=msg))
            print()

    return all_results

# ═══════════════════════════════════════════════════════════════════════
# ANÁLISIS ESTADÍSTICO
# ═══════════════════════════════════════════════════════════════════════

def aggregate(results: List[BenchmarkResult]) -> List[AggregatedStats]:
    """Calcula estadísticas agregadas por (solver, difficulty)."""
    groups:   Dict[tuple, List[float]] = {}
    timeouts: Dict[tuple, int]         = {}
    totals:   Dict[tuple, int]         = {}

    for r in results:
        key = (r.solver, r.difficulty)
        totals[key]  = totals.get(key, 0) + 1
        if r.timeout:
            timeouts[key] = timeouts.get(key, 0) + 1
        if r.success and not r.timeout:
            groups.setdefault(key, []).append(r.time_ms)

    stats = []
    for (solver, diff), times in groups.items():
        n = len(times)
        key = (solver, diff)
        stats.append(AggregatedStats(
            solver=solver, difficulty=diff, count=n,
            mean_ms=statistics.mean(times),
            median_ms=statistics.median(times),
            min_ms=min(times), max_ms=max(times),
            stddev_ms=statistics.stdev(times) if n > 1 else 0.0,
            success_rate=n / totals[key],
            timeout_count=timeouts.get(key, 0),
        ))
    return stats

def print_stats(stats: List[AggregatedStats]):
    ph("ESTADÍSTICAS AGREGADAS  [CPU time, ms]")
    for diff in ["easy", "medium", "hard"]:
        ds = sorted([s for s in stats if s.difficulty == diff], key=lambda s: s.median_ms)
        if not ds:
            continue
        print(f"\n{C.BOLD}{C.CYAN}  {'─'*70}\n  {diff.upper()}\n  {'─'*70}{C.ENDC}\n")
        hdr = (f"  {'Solver':<22} {'N':>4}  {'Median':>11}  {'Mean':>11}"
               f"  {'Min':>10}  {'Max':>11}  {'σ':>10}  {'OK%':>5}")
        print(f"{C.DIM}{hdr}{C.ENDC}")
        print(f"  {'─'*22}  {'─'*4}  {'─'*11}  {'─'*11}  {'─'*10}  {'─'*11}  {'─'*10}  {'─'*5}")
        for s in ds:
            print(f"  {s.solver:<22} {s.count:>4}  "
                  f"{s.median_ms:>10.4f}ms  {s.mean_ms:>10.4f}ms  "
                  f"{s.min_ms:>9.4f}ms  {s.max_ms:>10.4f}ms  "
                  f"{s.stddev_ms:>9.4f}ms  {s.success_rate*100:>4.0f}%")

def print_speedups(stats: List[AggregatedStats]):
    """Tabla de speedups entre pares de solvers, usando mediana de CPU time."""
    ph("FACTORES DE SPEEDUP  [mediana CPU time]")
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
            t_slow = ds.get(slow, 0)
            t_fast = ds.get(fast, 0)
            if t_slow > 0 and t_fast > 0:
                factor = t_slow / t_fast
                f_abs  = factor if factor >= 1 else 1 / factor
                direc  = "más rápido" if factor >= 1 else "más lento"
                bar    = "█" * min(int(f_abs), 40)
                print(f"    {label:<40}  {f_abs:>6.1f}× {direc}  {C.CYAN}{bar}{C.ENDC}")
            elif slow in ds and fast in ds:
                print(f"    {label:<40}  {C.YELLOW}N/A — tiempos demasiado pequeños{C.ENDC}")

# ═══════════════════════════════════════════════════════════════════════
# GUARDADO DE RESULTADOS
# ═══════════════════════════════════════════════════════════════════════

def save_results(results: List[BenchmarkResult], stats: List[AggregatedStats]):
    """Exporta resultados en CSV (detallado + estadísticas) y JSON."""
    # CSV detallado (un resultado por fila)
    csv_path = RESULTS_DIR / "benchmark_results.csv"
    with open(csv_path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=[
            "solver", "difficulty", "puzzle_num", "puzzle",
            "time_ms", "success", "timeout", "error"])
        w.writeheader()
        for r in results:
            row = asdict(r)
            row.pop("all_times", None)
            w.writerow(row)
    ok(f"CSV detallado:    {csv_path}")

    # CSV estadísticas agregadas
    stats_path = RESULTS_DIR / "statistics.csv"
    with open(stats_path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(AggregatedStats.__dataclass_fields__.keys()))
        w.writeheader()
        w.writerows([asdict(s) for s in stats])
    ok(f"CSV estadísticas: {stats_path}")

    # JSON completo con metadata
    json_path = RESULTS_DIR / "results.json"
    with open(json_path, "w") as f:
        json.dump({
            "metadata": {
                "runs_per_puzzle": RUNS_PER_PUZZLE,
                "warmup_runs":     WARMUP_RUNS,
                "timeout_s":       TIMEOUT,
                "time_metric":     "cpu_time_ms",
                "timestamp":       time.strftime("%Y-%m-%dT%H:%M:%S"),
            },
            "results":    [asdict(r) for r in results],
            "statistics": [asdict(s) for s in stats],
        }, f, indent=2)
    ok(f"JSON:             {json_path}")

# ═══════════════════════════════════════════════════════════════════════
# VISUALIZACIÓN
# ═══════════════════════════════════════════════════════════════════════

SOLVER_LABELS = {
    "prolog_naive":  "Prolog\nNaive BT",   "prolog_mrv":    "Prolog\nMRV",
    "prolog_clp":    "Prolog\nCLP(FD)",    "haskell_first": "Haskell\nFirstEmpty",
    "haskell_mrv":   "Haskell\nMRV",       "haskell_prop":  "Haskell\nPropMRV",
}
SOLVER_COLORS = {
    "prolog_naive":  "#E74C3C",  "prolog_mrv":    "#E67E22",
    "prolog_clp":    "#27AE60",  "haskell_first": "#2980B9",
    "haskell_mrv":   "#8E44AD",  "haskell_prop":  "#16A085",
}

def generate_plots(stats: List[AggregatedStats], results: List[BenchmarkResult]):
    """
    Genera 4 figuras PNG:
      1. Barras agrupadas — escala lineal
      2. Barras agrupadas — escala logarítmica
      3. Box plot de distribuciones (todas las dificultades)
      4. Heatmap de speedup relativo (dificultad medium)
    Todos los tiempos son CPU time en ms.
    """
    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
        import matplotlib.patches as mpatches
        import numpy as np
    except ImportError:
        warn("matplotlib no disponible — pip install matplotlib")
        return

    DIFFICULTIES  = ["easy", "medium", "hard"]
    SOLVER_ORDER  = [s[0] for s in SOLVERS]
    N             = len(SOLVER_ORDER)
    legend        = [mpatches.Patch(color="#E74C3C", label="Prolog"),
                     mpatches.Patch(color="#2980B9", label="Haskell")]

    def _bar_chart(ax, diff, yscale="linear"):
        ds     = {s.solver: s.median_ms for s in stats if s.difficulty == diff}
        x      = np.arange(N)
        vals   = [max(ds.get(sid, 0), 0.001 if yscale == "log" else 0) for sid in SOLVER_ORDER]
        colors = [SOLVER_COLORS[sid] for sid in SOLVER_ORDER]
        bars   = ax.bar(x, vals, color=colors, width=0.6, edgecolor="white", linewidth=0.8)
        ax.set_xticks(x)
        ax.set_xticklabels([SOLVER_LABELS[s] for s in SOLVER_ORDER], fontsize=7)
        ax.set_ylabel("CPU time (ms)" + (" — log" if yscale == "log" else ""))
        ax.set_title(diff.capitalize())
        ax.grid(axis="y", alpha=0.3, which="both" if yscale == "log" else "major")
        if yscale == "log":
            ax.set_yscale("log")
        else:
            for bar, v in zip(bars, vals):
                if v > 0:
                    ax.text(bar.get_x() + bar.get_width() / 2, bar.get_height(),
                            f"{v:.3f}", ha="center", va="bottom", fontsize=6)

    # ── Fig 1: lineal ────────────────────────────────────────────────────
    fig, axes = plt.subplots(1, 3, figsize=(18, 6))
    fig.suptitle("CPU time mediano por solver y dificultad", fontsize=14, fontweight="bold")
    for ax, diff in zip(axes, DIFFICULTIES):
        _bar_chart(ax, diff, yscale="linear")
    fig.legend(handles=legend, loc="lower center", ncol=2, fontsize=9, bbox_to_anchor=(0.5, -0.02))
    plt.tight_layout(rect=[0, 0.04, 1, 1])
    out = RESULTS_DIR / "fig1_bar_linear.png"
    plt.savefig(out, dpi=150, bbox_inches="tight"); plt.close()
    ok(f"Gráfico 1 → {out}")

    # ── Fig 2: logarítmica ───────────────────────────────────────────────
    fig, axes = plt.subplots(1, 3, figsize=(18, 6))
    fig.suptitle("CPU time mediano — escala logarítmica", fontsize=14, fontweight="bold")
    for ax, diff in zip(axes, DIFFICULTIES):
        _bar_chart(ax, diff, yscale="log")
    fig.legend(handles=legend, loc="lower center", ncol=2, fontsize=9, bbox_to_anchor=(0.5, -0.02))
    plt.tight_layout(rect=[0, 0.04, 1, 1])
    out = RESULTS_DIR / "fig2_bar_log.png"
    plt.savefig(out, dpi=150, bbox_inches="tight"); plt.close()
    ok(f"Gráfico 2 → {out}")

    # ── Fig 3: box plot ──────────────────────────────────────────────────
    raw: Dict[str, List[float]] = {sid: [] for sid in SOLVER_ORDER}
    for r in results:
        if r.success and not r.timeout:
            raw[r.solver].extend(r.all_times if r.all_times else [r.time_ms])

    fig, ax = plt.subplots(figsize=(12, 6))
    bp = ax.boxplot(
        [raw[sid] for sid in SOLVER_ORDER],
        tick_labels=[SOLVER_LABELS[sid].replace("\n", " ") for sid in SOLVER_ORDER],
        patch_artist=True,
        medianprops=dict(color="black", linewidth=2),
    )
    for patch, sid in zip(bp["boxes"], SOLVER_ORDER):
        patch.set_facecolor(SOLVER_COLORS[sid]); patch.set_alpha(0.7)
    ax.set_yscale("log")
    ax.set_ylabel("CPU time (ms) — log")
    ax.set_title("Distribución de CPU time (todas las dificultades)")
    ax.grid(axis="y", alpha=0.3, which="both")
    plt.xticks(rotation=15); plt.tight_layout()
    out = RESULTS_DIR / "fig3_boxplot.png"
    plt.savefig(out, dpi=150, bbox_inches="tight"); plt.close()
    ok(f"Gráfico 3 → {out}")

    # ── Fig 4: heatmap speedup ───────────────────────────────────────────
    med = {s.solver: s.median_ms for s in stats if s.difficulty == "medium"}
    if len(med) >= 2:
        matrix = np.zeros((N, N))
        for i, si in enumerate(SOLVER_ORDER):
            for j, sj in enumerate(SOLVER_ORDER):
                ti, tj = med.get(si, 0), med.get(sj, 0)
                if ti > 0 and tj > 0:
                    matrix[i, j] = ti / tj

        fig, ax = plt.subplots(figsize=(9, 7))
        im = ax.imshow(matrix, cmap="RdYlGn", aspect="auto",
                       norm=matplotlib.colors.LogNorm(vmin=0.1, vmax=max(matrix.max(), 1.1)))
        fig.colorbar(im, ax=ax).set_label("Speedup (fila / columna)")

        labels = [SOLVER_LABELS[s].replace("\n", " ") for s in SOLVER_ORDER]
        ax.set_xticks(range(N)); ax.set_xticklabels(labels, rotation=30, ha="right", fontsize=8)
        ax.set_yticks(range(N)); ax.set_yticklabels(labels, fontsize=8)
        ax.set_title("Speedup relativo — dificultad Media  [CPU time]\n"
                     "(valor > 1 → fila más rápida que columna)")
        for i in range(N):
            for j in range(N):
                v = matrix[i, j]
                ax.text(j, i, f"{v:.2f}×" if v > 0 else "—",
                        ha="center", va="center", fontsize=7,
                        color="black" if 0.4 < v < 4 else "white")
        plt.tight_layout()
        out = RESULTS_DIR / "fig4_heatmap_speedup.png"
        plt.savefig(out, dpi=150, bbox_inches="tight"); plt.close()
        ok(f"Gráfico 4 → {out}")

# ═══════════════════════════════════════════════════════════════════════
# VERIFICACIÓN DEL ENTORNO
# ═══════════════════════════════════════════════════════════════════════

def check_environment() -> bool:
    """Verifica disponibilidad de SWI-Prolog, fuentes .pl, ejecutable Haskell y puzzles."""
    ph("VERIFICANDO ENTORNO")
    all_ok = True

    found, stdout, _, _ = run_process(["swipl", "--version"], 10)
    if found:
        ok(f"SWI-Prolog: {stdout.splitlines()[0]}")
    else:
        err("SWI-Prolog no encontrado  →  sudo apt install swi-prolog")
        all_ok = False

    for name in ["sudoku_manual.pl", "sudoku_manual_mrv.pl", "sudoku_clp.pl"]:
        p = PROLOG_SRC / name
        if p.exists(): ok(f"Prolog source: {name}")
        else:          err(f"Prolog source FALTANTE: {p}"); all_ok = False

    exe = _find_haskell_exe()
    if exe: ok(f"Haskell exe: {exe}")
    else:   warn("Ejecutable Haskell no encontrado  →  cd haskell && stack build")

    for diff in ["easy", "medium", "hard"]:
        p = PUZZLES_DIR / diff / "puzzles.txt"
        if p.exists():
            with open(p) as f:
                n = sum(1 for l in f if len(l.strip()) == 81)
            ok(f"Puzzles {diff}: {n}")
        else:
            err(f"Puzzles FALTANTES: {p}"); all_ok = False

    try:
        import matplotlib; ok(f"matplotlib {matplotlib.__version__}")
    except ImportError:
        warn("matplotlib no instalado  →  pip install matplotlib")

    return all_ok

# ═══════════════════════════════════════════════════════════════════════
# CLI
# ═══════════════════════════════════════════════════════════════════════

def main():
    global RUNS_PER_PUZZLE

    parser = argparse.ArgumentParser(
        description="Benchmark comparativo Prolog vs Haskell — Sudoku Solvers",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Ejemplos:
  python comparison.py                              # benchmark completo
  python comparison.py --difficulty easy            # solo fácil
  python comparison.py --solvers prolog_clp         # un solver
  python comparison.py --quick                      # 1 puzzle, testing rápido
  python comparison.py --plot                       # + gráficos PNG
  python comparison.py --check                      # verificar entorno
        """)

    parser.add_argument("--difficulty", nargs="+", default=["easy", "medium", "hard"],
                        choices=["easy", "medium", "hard"], metavar="LEVEL",
                        help="Niveles a probar (default: todos)")
    parser.add_argument("--solvers", nargs="+", default=None,
                        choices=[s[0] for s in SOLVERS], metavar="SOLVER_ID",
                        help="Solvers a ejecutar (default: todos)")
    parser.add_argument("--quick",  action="store_true",
                        help="1 puzzle por dificultad, 1 run (testing)")
    parser.add_argument("--plot",   action="store_true",
                        help="Generar gráficos PNG (requiere matplotlib)")
    parser.add_argument("--check",  action="store_true",
                        help="Solo verificar entorno y salir")
    parser.add_argument("--runs",   type=int, default=RUNS_PER_PUZZLE,
                        help=f"Runs por solver×puzzle (default: {RUNS_PER_PUZZLE})")

    args    = parser.parse_args()
    env_ok  = check_environment()

    if args.check:
        sys.exit(0 if env_ok else 1)

    RUNS_PER_PUZZLE = 1 if args.quick else args.runs
    max_puzzles     = 1 if args.quick else None

    if not env_ok:
        warn("Hay problemas en el entorno. Continuando de todas formas...")

    results = run_full_benchmark(args.difficulty, args.solvers, max_puzzles)
    if not results:
        err("Sin resultados. ¿Están los puzzles y solvers disponibles?")
        sys.exit(1)

    stats = aggregate(results)
    print_stats(stats)
    print_speedups(stats)
    save_results(results, stats)

    if args.plot:
        generate_plots(stats, results)

    ph("BENCHMARK COMPLETADO")
    ok(f"Resultados en: {RESULTS_DIR}")

if __name__ == "__main__":
    main()