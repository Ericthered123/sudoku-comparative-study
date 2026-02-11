% ============================================================================
% SUITE DE TESTS PARA SUDOKU SOLVER
% ============================================================================
% Autores: [Tu nombre] y [Compañero]
% Fecha: Febrero 2026
%
% Descripción:
%   Conjunto de tests para verificar el correcto funcionamiento
%   de ambas implementaciones del solucionador de Sudoku.
%
% Uso:
%   ?- [tests].
%   ?- run_all_tests.
% ============================================================================

:- use_module(library(clpfd)).

% Cargar ambas implementaciones
:- consult(sudoku_clp).
:- consult(sudoku_manual).

% ----------------------------------------------------------------------------
% SUDOKUS DE PRUEBA
% ----------------------------------------------------------------------------

% Test 1: Sudoku muy fácil (muchos números dados)
test_muy_facil_matriz([
    [5,3,4,6,7,8,9,1,2],
    [6,7,2,1,9,5,3,4,8],
    [1,9,8,3,4,2,5,6,7],
    [8,5,9,7,6,1,4,2,3],
    [4,2,6,8,5,3,7,9,1],
    [7,1,3,9,2,4,8,5,6],
    [9,6,1,5,3,7,2,8,4],
    [2,8,7,4,1,9,6,3,5],
    [3,4,5,2,8,6,1,7,_]
]).

% Test 2: Sudoku fácil
test_facil_matriz([
    [5,3,_,_,7,_,_,_,_],
    [6,_,_,1,9,5,_,_,_],
    [_,9,8,_,_,_,_,6,_],
    [8,_,_,_,6,_,_,_,3],
    [4,_,_,8,_,3,_,_,1],
    [7,_,_,_,2,_,_,_,6],
    [_,6,_,_,_,_,2,8,_],
    [_,_,_,4,1,9,_,_,5],
    [_,_,_,_,8,_,_,7,9]
]).

% Test 3: Sudoku medio
test_medio_matriz([
    [_,2,_,6,_,8,_,_,_],
    [5,8,_,_,_,9,7,_,_],
    [_,_,_,_,4,_,_,_,_],
    [3,7,_,_,_,_,5,_,_],
    [6,_,_,_,_,_,_,_,4],
    [_,_,8,_,_,_,_,1,3],
    [_,_,_,_,2,_,_,_,_],
    [_,_,9,8,_,_,_,3,6],
    [_,_,_,3,_,6,_,9,_]
]).

% Test 4: Sudoku difícil (para CLP solamente, muy lento en manual)
test_dificil_matriz([
    [_,_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_,_]
]).

% Convertir matriz a lista plana (para versión manual)
test_facil_lista([
    5,3,x,x,7,x,x,x,x,
    6,x,x,1,9,5,x,x,x,
    x,9,8,x,x,x,x,6,x,
    8,x,x,x,6,x,x,x,3,
    4,x,x,8,x,3,x,x,1,
    7,x,x,x,2,x,x,x,6,
    x,6,x,x,x,x,2,8,x,
    x,x,x,4,1,9,x,x,5,
    x,x,x,x,8,x,x,7,9
]).

test_medio_lista([
    x,2,x,6,x,8,x,x,x,
    5,8,x,x,x,9,7,x,x,
    x,x,x,x,4,x,x,x,x,
    3,7,x,x,x,x,5,x,x,
    6,x,x,x,x,x,x,x,4,
    x,x,8,x,x,x,x,1,3,
    x,x,x,x,2,x,x,x,x,
    x,x,9,8,x,x,x,3,6,
    x,x,x,3,x,6,x,9,x
]).

% ----------------------------------------------------------------------------
% VERIFICADORES DE SOLUCIÓN
% ----------------------------------------------------------------------------

% verificar_solucion(+Sudoku)
%
% Verifica que un Sudoku resuelto sea válido:
% - Todas las celdas tienen valores 1-9
% - No hay duplicados en filas, columnas ni cuadrados
verificar_solucion_matriz(Sudoku) :-
    append(Sudoku, Lista),
    % Verificar que todos sean números entre 1 y 9
    maplist(between(1,9), Lista),
    % Verificar filas
    maplist(all_distinct, Sudoku),
    % Verificar columnas
    transpose(Sudoku, Columnas),
    maplist(all_distinct, Columnas),
    % Verificar bloques
    bloques(Sudoku, Bloques),
    maplist(all_distinct, Bloques).

% ----------------------------------------------------------------------------
% EJECUTORES DE TESTS
% ----------------------------------------------------------------------------

% test_clpfd(+Nombre, +Sudoku)
%
% Ejecuta un test con la versión CLP(FD)
test_clpfd(Nombre, Sudoku) :-
    write('>>> Test CLP(FD): '), write(Nombre), nl,
    statistics(walltime, [Start|_]),
    (   sudoku(Sudoku)
    ->  statistics(walltime, [End|_]),
        Tiempo is End - Start,
        write('     Resuelto en '), write(Tiempo), write(' ms'), nl,
        (   verificar_solucion_matriz(Sudoku)
        ->  write('     Solucion verificada'), nl
        ;   write('     ERROR: Solucion invalida'), nl, fail
        )
    ;   write('     FALLO: No se encontro solucion'), nl, fail
    ), nl.

% test_manual(+Nombre, +Sudoku)
%
% Ejecuta un test con la versión manual
% ADVERTENCIA: Puede ser MUY lento
test_manual(Nombre, Sudoku) :-
    write('>>> Test Manual: '), write(Nombre), nl,
    write('    (Esto puede tardar varios segundos...)'), nl,
    statistics(walltime, [Start|_]),
    (   sudoku(Sudoku, Solucion)
    ->  statistics(walltime, [End|_]),
        Tiempo is End - Start,
        write('     Resuelto en '), write(Tiempo), write(' ms'), nl,
        % Verificar que todos los valores sean números
        (   maplist(number, Solucion)
        ->  write('     Solucion completa'), nl
        ;   write('     ERROR: Solucion incompleta'), nl, fail
        )
    ;   write('     FALLO: No se encontró solucion'), nl, fail
    ), nl.

% ----------------------------------------------------------------------------
% SUITE COMPLETA DE TESTS
% ----------------------------------------------------------------------------

% run_all_tests
%
% Ejecuta todos los tests disponibles
run_all_tests :-
    nl,
    write('========================================'), nl,
    write('   SUITE DE TESTS - SUDOKU SOLVER'), nl,
    write('========================================'), nl, nl,

    % Obtener los sudokus de prueba PRIMERO
    test_muy_facil_matriz(Sudoku1),
    test_facil_matriz(Sudoku2),
    test_medio_matriz(Sudoku3),
    test_facil_lista(SudokuManual),
    test_medio_lista(SudokuManual2),

    write('--- TESTS CON CLP(FD) ---'), nl, nl,
    test_clpfd('Muy Facil', Sudoku1),
    test_clpfd('Facil', Sudoku2),
    test_clpfd('Medio', Sudoku3),

    write('--- TESTS CON VERSION MANUAL ---'), nl,
  %  write('NOTA: Solo ejecutamos el test facil'), nl,
    write('NOTA: El test medio puede tardar MUCHO'), nl, nl,
    test_manual('Facil', SudokuManual),
    test_manual('Medio',SudokuManual2),

    write('========================================'), nl,
    write('   TODOS LOS TESTS COMPLETADOS'), nl,
    write('========================================'), nl.

% run_clpfd_tests
%
% Solo ejecuta tests de CLP(FD) (más rápido)
run_clpfd_tests :-
    nl,
    write('========================================'), nl,
    write('   TESTS CLP(FD) - RÁPIDOS'), nl,
    write('========================================'), nl, nl,

    test_muy_facil_matriz(S1),
    test_facil_matriz(S2),
    test_medio_matriz(S3),

    test_clpfd('Muy Fácil', S1),
    test_clpfd('Fácil', S2),
    test_clpfd('Medio', S3),

    write('========================================'), nl,
    write('   TESTS CLP(FD) COMPLETADOS'), nl,
    write('========================================'), nl.

% benchmark_comparison
%
% Compara el rendimiento de ambas versiones en el mismo sudoku
benchmark_comparison :-
    nl,
    write('========================================'), nl,
    write('   COMPARACION DE RENDIMIENTO'), nl,
    write('========================================'), nl, nl,
    
    test_facil_matriz(SudokuCLP),
    test_facil_lista(SudokuManual),
    
    write('>>> Resolviendo con CLP(FD)...'), nl,
    statistics(walltime, [Start1|_]),
    sudoku(SudokuCLP),
    statistics(walltime, [End1|_]),
    TiempoCLP is End1 - Start1,
    write('    Tiempo: '), write(TiempoCLP), write(' ms'), nl, nl,
    
    write('>>> Resolviendo con version Manual...'), nl,
    write('    (Puede tardar bastante...)'), nl,
    statistics(walltime, [Start2|_]),
    sudoku(SudokuManual, _),
    statistics(walltime, [End2|_]),
    TiempoManual is End2 - Start2,
    write('    Tiempo: '), write(TiempoManual), write(' ms'), nl, nl,
    
    Diferencia is TiempoManual / TiempoCLP,
    write('========================================'), nl,
    write('CLP(FD) es '), write(Diferencia), write('x mas rapido'), nl,
    write('========================================'), nl.

% ----------------------------------------------------------------------------
% INSTRUCCIONES DE USO
% ----------------------------------------------------------------------------

:- write('========================================'), nl,
   write('Tests cargados exitosamente!'), nl,
   write('========================================'), nl,
   write('Comandos disponibles:'), nl,
   write('  run_all_tests.         - Ejecutar todos los tests'), nl,
   write('  run_clpfd_tests.       - Solo tests CLP(FD) (rápido)'), nl,
   write('  benchmark_comparison.  - Comparar rendimiento'), nl,
   write('========================================'), nl, nl.