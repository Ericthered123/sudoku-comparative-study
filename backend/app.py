"""
╔══════════════════════════════════════════════════════════════════╗
║  BACKEND API — Sudoku Comparative Study                          ║
║  Flask API para resolver puzzles con los 6 solvers               ║
╚══════════════════════════════════════════════════════════════════╝

Rutas:
  POST /api/solve          { puzzle: "81chars" } → resultados 6 solvers
  POST /api/solve_one      { puzzle: "81chars", solver: id } → resultado 1 solver
  GET  /api/puzzles        ?difficulty=easy|medium|hard → lista de puzzles
  GET  /api/health         → estado de swipl y haskell-exe

Instalar: pip install flask flask-cors
Ejecutar: python backend/app.py
"""

import os
import re
import sys
import subprocess
import tempfile
import statistics
from pathlib import Path
from flask import Flask, request, jsonify, send_from_directory
from flask_cors import CORS

app = Flask(__name__)
CORS(app)

FRONTEND_DIR = Path(__file__).parent.parent / "frontend"


@app.route('/', methods=['GET'])
def serve_frontend():
    return send_from_directory(FRONTEND_DIR, 'index.html')


def is_solvable(puzzle: str) -> bool:
    grid = [int(c) for c in puzzle]

    def possible(pos, val):
        row, col = divmod(pos, 9)
        box_r, box_c = (row // 3) * 3, (col // 3) * 3
        for i in range(9):
            if grid[row * 9 + i] == val: return False
            if grid[i * 9 + col] == val: return False
        for dr in range(3):
            for dc in range(3):
                if grid[(box_r + dr) * 9 + (box_c + dc)] == val: return False
        return True

    def solve():
        try:
            pos = grid.index(0)
        except ValueError:
            return True
        for val in range(1, 10):
            if possible(pos, val):
                grid[pos] = val
                if solve(): return True
                grid[pos] = 0
        return False

    return solve()

# ── Rutas del proyecto ───────────────────────────────────────────────────────

PROJECT_ROOT = Path(__file__).parent.parent
PUZZLES_DIR  = PROJECT_ROOT / "benchmarks" / "puzzles"
PROLOG_SRC   = PROJECT_ROOT / "prolog" / "src"
HASKELL_DIR  = PROJECT_ROOT / "haskell"
TIMEOUT      = 30   # segundos por solver

# ── Helpers de proceso (igual que comparison.py) ─────────────────────────────

def run_process(cmd, timeout):
    """Ejecuta proceso externo. Retorna (success, stdout, stderr, timed_out)."""
    try:
        proc = subprocess.Popen(
            cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
        )
        stdout, stderr = proc.communicate(timeout=timeout)
        return proc.returncode == 0, stdout.strip(), stderr.strip(), False
    except subprocess.TimeoutExpired:
        proc.kill()
        proc.communicate()
        return False, "", "TIMEOUT", True
    except FileNotFoundError as e:
        # En Windows, Popen lanza FileNotFoundError si el binario no está en PATH
        return False, "", str(e), False


def _find_haskell_exe():
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


def load_puzzles(difficulty):
    """Carga puzzles desde archivo; retorna lista de strings de 81 chars."""
    path = PUZZLES_DIR / difficulty / "puzzles.txt"
    if not path.exists():
        return []
    with open(path) as f:
        return [l.strip() for l in f if len(l.strip()) == 81]

# ── Conversión de puzzles ────────────────────────────────────────────────────

def puzzle_to_prolog_list(puzzle):
    """Lista Prolog con 'x' para vacíos (Naive/MRV)."""
    tokens = ['x' if c in '0.' else c for c in puzzle]
    return '[' + ','.join(tokens) + ']'

def puzzle_to_flat_list(puzzle):
    """Lista Prolog de enteros con 0 para vacíos (CLP)."""
    tokens = ['0' if c in '0.' else c for c in puzzle]
    return '[' + ','.join(tokens) + ']'

# ── Templates Prolog (con solución en línea 2) ───────────────────────────────

def _make_solve_script_naive(puzzle, src):
    return f""":- use_module(library(lists)).
:- consult('{src.as_posix()}').
:- initialization(main, main).
main :-
    Input = {puzzle_to_prolog_list(puzzle)},
    get_time(T1),
    ( sudoku(Input, Sol) -> true ; Sol = [] ),
    get_time(T2),
    Elapsed is (T2 - T1) * 1000.0,
    format('~6f~n', [Elapsed]),
    ( Sol \\= [] -> maplist(write, Sol), nl ; write('NO_SOLUTION'), nl ),
    halt.
"""

def _make_solve_script_mrv(puzzle, src):
    return f""":- use_module(library(lists)).
:- consult('{src.as_posix()}').
:- initialization(main, main).
main :-
    Input = {puzzle_to_prolog_list(puzzle)},
    get_time(T1),
    ( resolver(Input, Sol) -> true ; Sol = [] ),
    get_time(T2),
    Elapsed is (T2 - T1) * 1000.0,
    format('~6f~n', [Elapsed]),
    ( Sol \\= [] -> maplist(write, Sol), nl ; write('NO_SOLUTION'), nl ),
    halt.
"""

def _make_solve_script_clp(puzzle, src):
    return f""":- use_module(library(clpfd)).
:- use_module(library(lists)).
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
    append(Matrix, FlatSol),
    ( ground(FlatSol) -> maplist(write, FlatSol), nl ; write('NO_SOLUTION'), nl ),
    halt.
"""

# ── Ejecutor de scripts Prolog (parsea tiempo + solución) ────────────────────

def _run_prolog_solve_script(script_content):
    """
    Escribe script temporal, lo ejecuta con swipl, parsea dos líneas:
    línea 1 = tiempo en ms (float), línea 2 = solución de 81 chars.
    """
    tmp = None
    try:
        with tempfile.NamedTemporaryFile(
            mode='w', suffix='.pl', delete=False,
            encoding='utf-8', newline='\n'
        ) as f:
            f.write(script_content)
            tmp = Path(f.name)

        ok_flag, stdout, stderr, timed_out = run_process(
            ['swipl', '-q', tmp.as_posix()], TIMEOUT
        )

        if timed_out:
            return {"success": False, "timeout": True, "error": "Timeout"}

        lines = stdout.splitlines()
        if lines:
            m = re.match(r'([0-9]+(?:\.[0-9]+)?)', lines[0])
            if m:
                time_ms = float(m.group(1))
                sol_line = lines[1] if len(lines) > 1 else ""
                if sol_line == "NO_SOLUTION":
                    return {"success": False, "no_solution": True, "time_ms": time_ms}
                solution = sol_line if len(sol_line) == 81 else None
                return {"success": True, "time_ms": time_ms, "solution": solution}

        return {"success": False, "error": stderr[:200] if stderr else "No output"}

    finally:
        if tmp and tmp.exists():
            try:
                tmp.unlink()
            except Exception:
                pass

# ── Solvers ──────────────────────────────────────────────────────────────────

def solve_prolog(puzzle, solver_type):
    makers = {
        "naive": (_make_solve_script_naive, PROLOG_SRC / "sudoku_manual.pl"),
        "mrv":   (_make_solve_script_mrv,   PROLOG_SRC / "sudoku_manual_mrv.pl"),
        "clp":   (_make_solve_script_clp,   PROLOG_SRC / "sudoku_clp.pl"),
    }
    if solver_type not in makers:
        return {"success": False, "error": f"Solver desconocido: {solver_type}"}

    make_script, src = makers[solver_type]
    if not src.exists():
        return {"success": False, "error": f"Archivo no encontrado: {src}"}

    return _run_prolog_solve_script(make_script(puzzle, src))


def solve_haskell(puzzle, strat_key):
    exe = _find_haskell_exe()
    if exe is None:
        return {
            "success": False,
            "error": "Ejecutable Haskell no encontrado. Compilá: cd haskell && stack build"
        }

    ok_flag, stdout, stderr, timed_out = run_process(
        [str(exe), "--solve", strat_key, puzzle], TIMEOUT
    )

    if timed_out:
        return {"success": False, "timeout": True, "error": "Timeout"}

    lines = stdout.splitlines()
    if len(lines) >= 2:
        m = re.match(r'([0-9]+(?:\.[0-9]+)?)', lines[0])
        if m:
            time_ms = float(m.group(1)) * 1000.0   # segundos → ms
            sol = lines[1]
            if sol == "NO_SOLUTION":
                return {"success": False, "no_solution": True, "time_ms": time_ms}
            solution = sol if len(sol) == 81 else None
            return {"success": True, "time_ms": time_ms, "solution": solution}

    return {
        "success": False,
        "error": stderr[:200] if stderr else f"Salida inesperada: {stdout!r}"[:200]
    }

# ── Dispatch y validación compartidos ────────────────────────────────────────

SOLVER_DISPATCH = {
    "prolog_naive": lambda p: solve_prolog(p, "naive"),
    "prolog_mrv":   lambda p: solve_prolog(p, "mrv"),
    "prolog_clp":   lambda p: solve_prolog(p, "clp"),
    "haskell_fe":   lambda p: solve_haskell(p, "fe"),
    "haskell_mrv":  lambda p: solve_haskell(p, "mrv"),
    "haskell_pmrv": lambda p: solve_haskell(p, "pmrv"),
}


def _validate_puzzle(puzzle):
    """Valida y normaliza el puzzle. Retorna (puzzle_normalizado, error)."""
    if not puzzle or len(puzzle) != 81:
        return None, "puzzle debe tener exactamente 81 caracteres"
    if not re.match(r'^[0-9.]+$', puzzle):
        return None, "puzzle solo puede contener dígitos 0-9 o '.'"

    puzzle = puzzle.replace('.', '0')

    if not is_solvable(puzzle):
        return None, "El puzzle no tiene solución válida"

    return puzzle, None

# ── Rutas Flask ───────────────────────────────────────────────────────────────

@app.route('/api/health', methods=['GET'])
def api_health():
    swipl_ok, _, _, _ = run_process(['swipl', '--version'], 5)
    exe = _find_haskell_exe()
    return jsonify({
        "status": "ok",
        "swipl": swipl_ok,
        "haskell": exe is not None,
        "haskell_path": str(exe) if exe else None,
    })


@app.route('/api/puzzles', methods=['GET'])
def api_puzzles():
    difficulty = request.args.get('difficulty', 'easy')
    if difficulty not in ('easy', 'medium', 'hard'):
        return jsonify({"error": "difficulty debe ser easy, medium o hard"}), 400

    puzzles = load_puzzles(difficulty)
    return jsonify({
        "difficulty": difficulty,
        "puzzles": puzzles,
        "count": len(puzzles),
    })


@app.route('/api/solve', methods=['POST'])
def api_solve():
    data = request.get_json(force=True, silent=True) or {}
    puzzle, error = _validate_puzzle(data.get('puzzle', ''))
    if error:
        return jsonify({"error": error}), 400

    # Ejecutar los 6 solvers secuencialmente
    results = {}
    for solver_id, runner in SOLVER_DISPATCH.items():
        try:
            results[solver_id] = runner(puzzle)
        except Exception as e:
            results[solver_id] = {"success": False, "error": str(e)}

    return jsonify({"puzzle": puzzle, "results": results})


@app.route('/api/solve_one', methods=['POST'])
def api_solve_one():
    data = request.get_json(force=True, silent=True) or {}
    puzzle, error = _validate_puzzle(data.get('puzzle', ''))
    if error:
        return jsonify({"error": error}), 400

    solver_id = data.get('solver', '')
    runner = SOLVER_DISPATCH.get(solver_id)
    if runner is None:
        valid = ', '.join(SOLVER_DISPATCH)
        return jsonify({"error": f"solver debe ser uno de: {valid}"}), 400

    try:
        result = runner(puzzle)
    except Exception as e:
        result = {"success": False, "error": str(e)}

    return jsonify({"puzzle": puzzle, "solver": solver_id, "result": result})


if __name__ == '__main__':
    app.run(
        host='0.0.0.0',
        port=int(os.environ.get('PORT', 5000)),
        debug=os.environ.get('FLASK_DEBUG', '1') == '1',
    )
