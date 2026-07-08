"""Tests para el backend Flask (validación y dispatch de solvers).

Los runners reales (swipl / sudoku-exe) se reemplazan con fakes vía
monkeypatch: acá solo se prueba la lógica HTTP, no los solvers.
"""

import pytest

import app as backend


# Puzzle fácil válido (de CLAUDE.md)
VALID_PUZZLE = (
    "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
)

# Fila 1 = 1-8 y vacío en col 9; la celda (1,9) no admite ningún valor
# porque el 9 ya está en su columna → sin solución, falla rápido.
UNSOLVABLE_PUZZLE = "123456780" + "000000009" + "0" * 63

ALL_SOLVER_IDS = [
    "prolog_naive", "prolog_mrv", "prolog_clp",
    "haskell_fe", "haskell_mrv", "haskell_pmrv",
]


@pytest.fixture
def client():
    backend.app.config["TESTING"] = True
    with backend.app.test_client() as c:
        yield c


@pytest.fixture
def fake_solvers(monkeypatch):
    """Reemplaza SOLVER_DISPATCH con fakes que registran las llamadas."""
    calls = []

    def make_fake(solver_id):
        def fake(puzzle):
            calls.append((solver_id, puzzle))
            return {"success": True, "time_ms": 1.5, "solution": "9" * 81}
        return fake

    fake_dispatch = {sid: make_fake(sid) for sid in ALL_SOLVER_IDS}
    monkeypatch.setattr(backend, "SOLVER_DISPATCH", fake_dispatch)
    return calls


# ── Validación de /api/solve_one ─────────────────────────────────────────────

def test_solve_one_rejects_missing_puzzle(client):
    res = client.post("/api/solve_one", json={"solver": "prolog_naive"})
    assert res.status_code == 400
    assert "error" in res.get_json()


def test_solve_one_rejects_wrong_length(client):
    res = client.post("/api/solve_one", json={"puzzle": "123", "solver": "prolog_naive"})
    assert res.status_code == 400


def test_solve_one_rejects_bad_charset(client):
    bad = "x" * 81
    res = client.post("/api/solve_one", json={"puzzle": bad, "solver": "prolog_naive"})
    assert res.status_code == 400


def test_solve_one_rejects_unknown_solver(client, fake_solvers):
    res = client.post("/api/solve_one", json={"puzzle": VALID_PUZZLE, "solver": "cobol_bt"})
    assert res.status_code == 400
    assert fake_solvers == []


def test_solve_one_rejects_unsolvable_puzzle(client, fake_solvers):
    res = client.post(
        "/api/solve_one",
        json={"puzzle": UNSOLVABLE_PUZZLE, "solver": "prolog_naive"},
    )
    assert res.status_code == 400
    assert fake_solvers == []


# ── Dispatch de /api/solve_one ───────────────────────────────────────────────

def test_solve_one_calls_requested_solver_only(client, fake_solvers):
    res = client.post(
        "/api/solve_one",
        json={"puzzle": VALID_PUZZLE, "solver": "haskell_mrv"},
    )
    assert res.status_code == 200
    assert fake_solvers == [("haskell_mrv", VALID_PUZZLE)]


def test_solve_one_passes_through_result(client, fake_solvers):
    res = client.post(
        "/api/solve_one",
        json={"puzzle": VALID_PUZZLE, "solver": "prolog_clp"},
    )
    data = res.get_json()
    assert data["solver"] == "prolog_clp"
    assert data["result"] == {"success": True, "time_ms": 1.5, "solution": "9" * 81}


def test_solve_one_normalizes_dots(client, fake_solvers):
    dotted = VALID_PUZZLE.replace("0", ".")
    res = client.post(
        "/api/solve_one",
        json={"puzzle": dotted, "solver": "prolog_naive"},
    )
    assert res.status_code == 200
    # El solver debe recibir el puzzle normalizado con '0', no con '.'
    assert fake_solvers == [("prolog_naive", VALID_PUZZLE)]


# ── run_process: binario inexistente no debe explotar ───────────────────────

def test_run_process_handles_missing_binary():
    ok, stdout, stderr, timed_out = backend.run_process(
        ["binario-inexistente-xyz", "--version"], 5
    )
    assert ok is False
    assert timed_out is False
    assert stderr != ""


# ── Regresión: /api/solve sigue devolviendo los 6 solvers ────────────────────

def test_solve_all_returns_six_results(client, fake_solvers):
    res = client.post("/api/solve", json={"puzzle": VALID_PUZZLE})
    assert res.status_code == 200
    data = res.get_json()
    assert data["puzzle"] == VALID_PUZZLE
    assert sorted(data["results"].keys()) == sorted(ALL_SOLVER_IDS)
    assert len(fake_solvers) == 6


def test_solve_all_still_validates(client):
    res = client.post("/api/solve", json={"puzzle": "123"})
    assert res.status_code == 400
