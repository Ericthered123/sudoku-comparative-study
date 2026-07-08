---
title: Sudoku Comparative Study
emoji: 🧩
colorFrom: blue
colorTo: yellow
sdk: docker
app_port: 5000
pinned: false
---

# Sudoku Comparative Study

Comparative study of six Sudoku solvers — three in **Prolog** (Naive Backtracking,
MRV heuristic, CLP(FD)) and three in **Haskell** (FirstEmpty, MRV, Propagation+MRV) —
with a web UI to solve any puzzle with all six at once and compare their times.

Final project for _Programación Lógica y Funcional_, **UNNOBA** (2026).
Authors: Eric Doyle & Bruno Lodeiro.

## Quick start (Docker)

```bash
docker build -t sudoku-study .
docker run --rm -p 5000:5000 sudoku-study
# open http://localhost:5000
```

One container with SWI-Prolog, the compiled Haskell solver and the Flask API,
which also serves the frontend.

## Running locally without Docker

Requirements: Python 3 (`pip install flask flask-cors`), [SWI-Prolog](https://www.swi-prolog.org/)
on PATH, and [Stack](https://docs.haskellstack.org/) to build the Haskell solver.

```bash
cd haskell && stack build && cd ..
python backend/app.py          # API + frontend on http://localhost:5000
python -m pytest backend/     # backend tests (no swipl/stack needed)
```

## Project layout

| Path | Contents |
|---|---|
| `prolog/src/` | The three Prolog solvers |
| `haskell/src/` | The three Haskell strategies |
| `backend/` | Flask API (`/api/solve`, `/api/solve_one`, `/api/puzzles`, `/api/health`) |
| `frontend/` | Single-file web UI (light/dark, ES/EN, live per-solver results) |
| `benchmarks/` | Puzzle sets by difficulty + `comparison.py` benchmark runner |

## Benchmarks

```bash
python benchmarks/comparison.py --difficulty easy --max-puzzles 5
```

Results (CSV/JSON/PNG) land in `benchmarks/results/`. See `CLAUDE.md` for the
full command reference and `IMPROVEMENTS.md` for the roadmap.

> The YAML header above configures the Hugging Face **Docker Space** deployment;
> GitHub simply renders it as metadata.
