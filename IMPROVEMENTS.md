# Improvements Roadmap

Tracking list for pending project improvements. Ordered by priority.

## 1. Reproducibility — Docker 🐳 (in progress)

- [x] Multi-stage `Dockerfile`: build `sudoku-exe` with Stack (GHC 9.6.4), final image with SWI-Prolog + Python/Flask
- [x] Serve the frontend from Flask so one container exposes the whole app
- [ ] `docker build -t sudoku-study . && docker run -p 5000:5000 sudoku-study` documented in README
- [ ] (Optional) publish the image to GHCR via GitHub Actions

## 2. Benchmark rigor

- [ ] Multiple runs per solver (`runs: N` in the API / `--runs` in comparison.py); report median + std dev, discard warmup
- [ ] Instrument solvers to report **backtrack/node counts**, not just wall time (machine-independent comparison — key result for the report)
- [ ] Characterize puzzle difficulty by measured search effort, not clue count

## 3. Backend hardening

- [ ] Time-budget (or skip) the Python `is_solvable()` pre-check — it can run far longer than the solver timeout on pathological puzzles
- [ ] Gate `debug=True` behind an env var (Werkzeug debugger = RCE if ever exposed)
- [ ] Move `TIMEOUT`, host and port to env vars
- [ ] Add `requirements.txt`

## 4. Tests & CI

- [ ] GitHub Actions: run `pytest backend/` on push (no swipl/stack needed)
- [ ] Haskell QuickCheck properties: solver output satisfies all constraints; strategies agree on unique-solution puzzles
- [ ] Prolog `plunit` tests
- [ ] Playwright smoke test using the `?puzzle=…&autosolve=1` deep link

## 5. Docs & presentation

- [ ] Root README: screenshots (light + dark), setup steps, architecture sketch, summary results chart
- [ ] Write methodology/results/conclusions in `docs/` (feed from `benchmarks/results/`)
- [ ] LICENSE file

## 6. Deployment — Hugging Face Spaces (chosen)

- [x] README.md has the Docker Space frontmatter (`sdk: docker`, `app_port: 5000`)
- [x] Frontend API base is same-origin aware (works behind any host)
- [ ] Create the Space (type: Docker) at huggingface.co/new-space and push:
      `git remote add space https://huggingface.co/spaces/<user>/sudoku-comparative-study`
      `git push space main`
- [ ] (Optional) GitHub Action to auto-sync `main` → Space on every push

Notes: free tier = 2 vCPU / 16 GB, sleeps only after ~48 h idle (vs 15 min on
Render, which is why HF was chosen). GitHub Pages discarded: static only, no
backend. Benchmark numbers measured on shared cloud CPUs are noisy — use the
deployment for demos, measure locally for the report.

## 7. Frontend extras (later)

- [ ] Export results as CSV/JSON
- [ ] Run history in localStorage
- [ ] Side-by-side comparison of two puzzles
- [ ] Averaging over N runs in the UI (needs #2)
