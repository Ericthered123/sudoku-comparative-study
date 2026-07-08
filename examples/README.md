# Ejemplos

Ejemplos de uso de los solvers:

- **Prolog**: `../prolog/src/ejemplos.pl` — puzzles de ejemplo y consultas
  listas para copiar en el toplevel de SWI-Prolog.
- **Haskell**: `sudoku-exe --example easy|medium|hard` resuelve puzzles
  predefinidos desde la CLI (ver `../haskell/README.md`).
- **API**: con el backend corriendo,
  `curl -X POST localhost:5000/api/solve_one -H "Content-Type: application/json" -d '{"puzzle":"003020600900305001001806400008102900700000008006708200002609500800203009005010300","solver":"prolog_clp"}'`
- **UI**: deep link `http://localhost:5000/?puzzle=<81 chars>&autosolve=1`.
