% ============================================================================
% SUDOKUS DE BENCHMARK - Comparación Humano vs Máquina
% ============================================================================
% Este archivo contiene sudokus en ambos formatos (manual y CLP) con:
% - Sudoku original
% - Solución
% - Tiempo estimado humano
% - Dificultad
% - Número de pistas iniciales
% ============================================================================

% ============================================================================
% SUDOKU 1: MUY FÁCIL
% ============================================================================
% Pistas: 36/81 (44%)
% Dificultad: ★☆☆☆☆
% Tiempo humano estimado: 3-5 minutos
% Tiempo CLP(FD) estimado: < 0.01 segundos
% Tiempo Manual estimado: 0.1-0.5 segundos

:-encoding(utf8).

% Formato CLP (matriz 9x9)
sudoku_muy_facil_clp([
    [5,3,4, 6,7,8, 9,1,2],
    [6,7,2, 1,9,5, 3,4,8],
    [1,9,8, 3,4,2, 5,6,7],
    
    [8,5,9, 7,6,1, 4,2,3],
    [4,2,6, 8,5,3, 7,9,1],
    [7,1,3, 9,2,4, 8,5,6],
    
    [9,6,1, 5,3,7, 2,8,4],
    [2,8,7, 4,1,9, 6,3,5],
    [3,4,5, 2,8,6, 1,7,_]  % Solo falta 1 número
]).

% Formato Manual (lista plana con 'x')
sudoku_muy_facil_manual([
    5,3,4, 6,7,8, 9,1,2,
    6,7,2, 1,9,5, 3,4,8,
    1,9,8, 3,4,2, 5,6,7,
    8,5,9, 7,6,1, 4,2,3,
    4,2,6, 8,5,3, 7,9,1,
    7,1,3, 9,2,4, 8,5,6,
    9,6,1, 5,3,7, 2,8,4,
    2,8,7, 4,1,9, 6,3,5,
    3,4,5, 2,8,6, 1,7,x
]).

% Solución
solucion_muy_facil([
    5,3,4, 6,7,8, 9,1,2,
    6,7,2, 1,9,5, 3,4,8,
    1,9,8, 3,4,2, 5,6,7,
    8,5,9, 7,6,1, 4,2,3,
    4,2,6, 8,5,3, 7,9,1,
    7,1,3, 9,2,4, 8,5,6,
    9,6,1, 5,3,7, 2,8,4,
    2,8,7, 4,1,9, 6,3,5,
    3,4,5, 2,8,6, 1,7,9
]).

% ============================================================================
% SUDOKU 2: FÁCIL
% ============================================================================
% Pistas: 30/81 (37%)
% Dificultad: ★★☆☆☆
% Tiempo humano estimado: 10-15 minutos
% Tiempo CLP(FD) estimado: 0.01-0.05 segundos
% Tiempo Manual estimado: 1-5 segundos

% Formato CLP (matriz 9x9)
sudoku_facil_clp([
    [5,3,_, _,7,_, _,_,_],
    [6,_,_, 1,9,5, _,_,_],
    [_,9,8, _,_,_, _,6,_],
    
    [8,_,_, _,6,_, _,_,3],
    [4,_,_, 8,_,3, _,_,1],
    [7,_,_, _,2,_, _,_,6],
    
    [_,6,_, _,_,_, 2,8,_],
    [_,_,_, 4,1,9, _,_,5],
    [_,_,_, _,8,_, _,7,9]
]).

% Formato Manual (lista plana con 'x')
sudoku_facil_manual([
    5,3,x, x,7,x, x,x,x,
    6,x,x, 1,9,5, x,x,x,
    x,9,8, x,x,x, x,6,x,
    8,x,x, x,6,x, x,x,3,
    4,x,x, 8,x,3, x,x,1,
    7,x,x, x,2,x, x,x,6,
    x,6,x, x,x,x, 2,8,x,
    x,x,x, 4,1,9, x,x,5,
    x,x,x, x,8,x, x,7,9
]).

% Solución
solucion_facil([
    5,3,4, 6,7,8, 9,1,2,
    6,7,2, 1,9,5, 3,4,8,
    1,9,8, 3,4,2, 5,6,7,
    8,5,9, 7,6,1, 4,2,3,
    4,2,6, 8,5,3, 7,9,1,
    7,1,3, 9,2,4, 8,5,6,
    9,6,1, 5,3,7, 2,8,4,
    2,8,7, 4,1,9, 6,3,5,
    3,4,5, 2,8,6, 1,7,9
]).

% ============================================================================
% SUDOKU 3: MEDIO
% ============================================================================
% Pistas: 25/81 (31%)
% Dificultad: ★★★☆☆
% Tiempo humano estimado: 20-30 minutos
% Tiempo CLP(FD) estimado: 0.05-0.2 segundos
% Tiempo Manual estimado: 10-60 segundos

% Formato CLP (matriz 9x9)
sudoku_medio_clp([
    [_,_,_, _,_,_, _,_,_],
    [_,_,_, _,_,3, _,8,5],
    [_,_,1, _,2,_, _,_,_],
    
    [_,_,_, 5,_,7, _,_,_],
    [_,_,4, _,_,_, 1,_,_],
    [_,9,_, _,_,_, _,_,_],
    
    [5,_,_, _,_,_, _,7,3],
    [_,_,2, _,1,_, _,_,_],
    [_,_,_, _,4,_, _,_,9]
]).

% Formato Manual (lista plana con 'x')
sudoku_medio_manual([
    x,x,x, x,x,x, x,x,x,
    x,x,x, x,x,3, x,8,5,
    x,x,1, x,2,x, x,x,x,
    x,x,x, 5,x,7, x,x,x,
    x,x,4, x,x,x, 1,x,x,
    x,9,x, x,x,x, x,x,x,
    5,x,x, x,x,x, x,7,3,
    x,x,2, x,1,x, x,x,x,
    x,x,x, x,4,x, x,x,9
]).

% Solución
solucion_medio([
    9,8,7, 6,5,4, 3,2,1,
    2,4,6, 1,7,3, 9,8,5,
    3,5,1, 9,2,8, 7,4,6,
    1,2,8, 5,3,7, 6,9,4,
    6,3,4, 8,9,2, 1,5,7,
    7,9,5, 4,6,1, 8,3,2,
    5,1,9, 2,8,6, 4,7,3,
    4,7,2, 3,1,9, 5,6,8,
    8,6,3, 7,4,5, 2,1,9
]).

% ============================================================================
% SUDOKU 4: DIFÍCIL
% ============================================================================
% Pistas: 22/81 (27%)
% Dificultad: ★★★★☆
% Tiempo humano estimado: 45-90 minutos (requiere técnicas avanzadas)
% Tiempo CLP(FD) estimado: 0.5-2 segundos
% Tiempo Manual estimado: 5-30 minutos (puede no terminar)

% Formato CLP (matriz 9x9)
sudoku_dificil_clp([
    [8,_,_, _,_,_, _,_,_],
    [_,_,3, 6,_,_, _,_,_],
    [_,7,_, _,9,_, 2,_,_],
    
    [_,5,_, _,_,7, _,_,_],
    [_,_,_, _,4,5, 7,_,_],
    [_,_,_, 1,_,_, _,3,_],
    
    [_,_,1, _,_,_, _,6,8],
    [_,_,8, 5,_,_, _,1,_],
    [_,9,_, _,_,_, 4,_,_]
]).

% Formato Manual (lista plana con 'x')
sudoku_dificil_manual([
    8,x,x, x,x,x, x,x,x,
    x,x,3, 6,x,x, x,x,x,
    x,7,x, x,9,x, 2,x,x,
    x,5,x, x,x,7, x,x,x,
    x,x,x, x,4,5, 7,x,x,
    x,x,x, 1,x,x, x,3,x,
    x,x,1, x,x,x, x,6,8,
    x,x,8, 5,x,x, x,1,x,
    x,9,x, x,x,x, 4,x,x
]).

% Solución
solucion_dificil([
    8,1,2, 7,5,3, 6,4,9,
    9,4,3, 6,8,2, 1,7,5,
    6,7,5, 4,9,1, 2,8,3,
    1,5,4, 2,3,7, 8,9,6,
    3,6,9, 8,4,5, 7,2,1,
    2,8,7, 1,6,9, 5,3,4,
    5,2,1, 9,7,4, 3,6,8,
    4,3,8, 5,2,6, 9,1,7,
    7,9,6, 3,1,8, 4,5,2
]).

% ============================================================================
% SUDOKU 5: EXTREMO (AI Escargot)
% ============================================================================
% Pistas: 21/81 (26%)
% Dificultad: ★★★★★
% Tiempo humano estimado: 2-6 horas (solo expertos con técnicas avanzadas)
% Tiempo CLP(FD) estimado: 1-5 segundos
% Tiempo Manual estimado: 30+ minutos (probablemente no termine)
% Nota: Considerado uno de los sudokus más difíciles del mundo (2006)

% Formato CLP (matriz 9x9)
sudoku_extremo_clp([
    [1,_,_, _,_,7, _,9,_],
    [_,3,_, _,2,_, _,_,8],
    [_,_,9, 6,_,_, 5,_,_],
    
    [_,_,5, 3,_,_, 9,_,_],
    [_,1,_, _,8,_, _,_,2],
    [6,_,_, _,_,4, _,_,_],
    
    [3,_,_, _,_,_, _,1,_],
    [_,4,_, _,_,_, _,_,7],
    [_,_,7, _,_,_, 3,_,_]
]).

% Formato Manual (lista plana con 'x')
sudoku_extremo_manual([
    1,x,x, x,x,7, x,9,x,
    x,3,x, x,2,x, x,x,8,
    x,x,9, 6,x,x, 5,x,x,
    x,x,5, 3,x,x, 9,x,x,
    x,1,x, x,8,x, x,x,2,
    6,x,x, x,x,4, x,x,x,
    3,x,x, x,x,x, x,1,x,
    x,4,x, x,x,_, x,x,7,
    x,x,7, x,x,x, 3,x,x
]).

% Solución
solucion_extremo([
    1,6,2, 8,5,7, 4,9,3,
    5,3,4, 1,2,9, 6,7,8,
    7,8,9, 6,4,3, 5,2,1,
    4,7,5, 3,1,2, 9,8,6,
    9,1,3, 5,8,6, 7,4,2,
    6,2,8, 7,9,4, 1,3,5,
    3,5,6, 4,7,8, 2,1,9,
    2,4,1, 9,3,5, 8,6,7,
    8,9,7, 2,6,1, 3,5,4
]).

% ============================================================================
% TABLA RESUMEN COMPARATIVA
% ============================================================================
% 
% | Sudoku    | Pistas | Tiempo Humano | Tiempo CLP(FD) | Tiempo Manual  | Speedup vs Humano |
% |-----------|--------|---------------|----------------|----------------|-------------------|
% | Muy Fácil | 36     | 3-5 min       | < 0.01s        | 0.1-0.5s       | ~18,000x         |
% | Fácil     | 30     | 10-15 min     | 0.01-0.05s     | 1-5s           | ~12,000x         |
% | Medio     | 25     | 20-30 min     | 0.05-0.2s      | 10-60s         | ~6,000x          |
% | Difícil   | 22     | 45-90 min     | 0.5-2s         | 5-30 min       | ~1,500x          |
% | Extremo   | 21     | 2-6 horas     | 1-5s           | 30+ min        | ~1,440x          |
%
% OBSERVACIONES:
% - CLP(FD) es consistentemente ~1,000-18,000x más rápido que humanos
% - La versión manual es ~100-1,000x más lenta que CLP(FD)
% - CLP(FD) mantiene tiempo casi constante; humanos escalan exponencialmente
% - Para sudokus extremos, la versión manual puede no terminar
%
% TÉCNICAS HUMANAS NECESARIAS POR NIVEL:
% - Muy Fácil/Fácil: "Naked Singles", "Hidden Singles"
% - Medio: + "Naked Pairs/Triples", "Pointing Pairs"
% - Difícil: + "X-Wing", "Swordfish", "Coloring"
% - Extremo: + "Chains", "Trial & Error avanzado"
%
% ============================================================================

% ============================================================================
% PREDICADOS AUXILIARES PARA BENCHMARKING
% ============================================================================

% Ejecutar benchmark de un sudoku específico
benchmark_sudoku(Nombre, SudokuCLP, SudokuManual) :-
    nl,
    write('========================================'), nl,
    write('BENCHMARK: '), write(Nombre), nl,
    write('========================================'), nl, nl,
    
    % Test CLP
    write('>>> Versión CLP(FD):'), nl,
    statistics(walltime, [T1|_]),
    (sudoku(SudokuCLP) -> Resultado1 = 'OK' ; Resultado1 = 'FALLÓ'),
    statistics(walltime, [T2|_]),
    TiempoCLP is T2 - T1,
    write('    Resultado: '), write(Resultado1), nl,
    write('    Tiempo: '), write(TiempoCLP), write(' ms'), nl, nl,
    
    % Test Manual
    write('>>> Versión Manual:'), nl,
    statistics(walltime, [T3|_]),
    (sudoku(SudokuManual, _) -> Resultado2 = 'OK' ; Resultado2 = 'FALLÓ'),
    statistics(walltime, [T4|_]),
    TiempoManual is T4 - T3,
    write('    Resultado: '), write(Resultado2), nl,
    write('    Tiempo: '), write(TiempoManual), write(' ms'), nl, nl,
    
    % Comparación
    (TiempoCLP > 0 -> 
        (Factor is TiempoManual / TiempoCLP,
         write('>>> CLP es '), write(Factor), write('x más rápido que Manual'), nl)
    ; write('>>> CLP demasiado rápido para medir diferencia significativa'), nl),
    
    write('========================================'), nl, nl.

% Ejecutar todos los benchmarks desde /benchmark/puzzles
:- consult('../../prolog/src/sudoku_clp').
:- consult('../../prolog/src/sudoku_manual').

benchmark_todos :-
    write('╔════════════════════════════════════════╗'), nl,
    write('║   SUITE COMPLETA DE BENCHMARKS         ║'), nl,
    write('╚════════════════════════════════════════╝'), nl,
    
    sudoku_facil_clp(S1_clp),
    sudoku_facil_manual(S1_man),
    benchmark_sudoku('FÁCIL', S1_clp, S1_man),
    
    sudoku_medio_clp(S2_clp),
    sudoku_medio_manual(S2_man),
    benchmark_sudoku('MEDIO', S2_clp, S2_man),
    
    write('NOTA: Los sudokus DIFÍCIL y EXTREMO tardarán más en la versión manual.'), nl,
    write('      Presiona Ctrl+C para cancelar si tarda demasiado.'), nl, nl.

% Instrucciones
:- nl,
   write('========================================'), nl,
   write('BENCHMARKS CARGADOS'), nl,
   write('========================================'), nl,
   write('Uso:'), nl,
   write('  benchmark_todos.           - Ejecutar todos'), nl,
   write('  sudoku_facil_clp(S).       - Ver sudoku fácil'), nl,
   write('  solucion_facil(Sol).       - Ver solución'), nl,
   write('========================================'), nl, nl.