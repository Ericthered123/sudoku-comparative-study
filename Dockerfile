# ── Stage 1: compilar el solver Haskell ──────────────────────────────────────
# El resolver lts-22.7 de stack.yaml usa GHC 9.6.4 (el comentario ahí dice 9.6.3,
# pero stack exige match exacto de versión menor)
FROM haskell:9.6.4-slim AS haskell-build

WORKDIR /build
COPY haskell/ .
RUN stack build --system-ghc --copy-bins --local-bin-path /build/bin

# ── Stage 2: runtime — Python + SWI-Prolog + binario Haskell ─────────────────
FROM python:3.12-slim-bookworm

RUN apt-get update \
    && apt-get install -y --no-install-recommends swi-prolog-nox \
    && rm -rf /var/lib/apt/lists/*

RUN pip install --no-cache-dir flask flask-cors

WORKDIR /app
COPY backend/ backend/
COPY frontend/ frontend/
COPY prolog/ prolog/
COPY benchmarks/puzzles/ benchmarks/puzzles/
COPY --from=haskell-build /build/bin/sudoku-exe /usr/local/bin/sudoku-exe

ENV FLASK_DEBUG=0
EXPOSE 5000

CMD ["python", "backend/app.py"]
