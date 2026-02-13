% ============================================================================
% EJEMPLOS DE USO - SUDOKU SOLVER
% ============================================================================
% Este archivo contiene ejemplos prácticos de cómo usar los solucionadores
% de Sudoku, tanto con CLP(FD) como con la versión manual.
% ============================================================================

% ----------------------------------------------------------------------------
% CÓMO CARGAR Y USAR
% ----------------------------------------------------------------------------

% 1. Iniciar SWI-Prolog:
%    $ swipl

% 2. Cargar este archivo:
%    ?- [ejemplos].

% 3. Ejecutar los ejemplos que aparecen más abajo

% ----------------------------------------------------------------------------
% EJEMPLOS CON CLP(FD) (VERSIÓN RECOMENDADA)
% ----------------------------------------------------------------------------

% Upload CLP Version (FD)

:- encoding(utf8).
:- consult(sudoku_clp).

% Ejemplo 1: Resolver un Sudoku simple y ver la solución
ejemplo_1 :-
    write('=== EJEMPLO 1: Sudoku simple ==='), nl,
    S = [[5,3,_,_,7,_,_,_,_],
         [6,_,_,1,9,5,_,_,_],
         [_,9,8,_,_,_,_,6,_],
         [8,_,_,_,6,_,_,_,3],
         [4,_,_,8,_,3,_,_,1],
         [7,_,_,_,2,_,_,_,6],
         [_,6,_,_,_,_,2,8,_],
         [_,_,_,4,1,9,_,_,5],
         [_,_,_,_,8,_,_,7,9]],
    sudoku(S),
    imprimir_sudoku_clp(S).

% Ejemplo 2: Medir el tiempo de resolución
ejemplo_2 :-
    write('=== EJEMPLO 2: Medición de tiempo ==='), nl,
    S = [[_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,3,_,8,5],
         [_,_,1,_,2,_,_,_,_],
         [_,_,_,5,_,7,_,_,_],
         [_,_,4,_,_,_,1,_,_],
         [_,9,_,_,_,_,_,_,_],
         [5,_,_,_,_,_,_,7,3],
         [_,_,2,_,1,_,_,_,_],
         [_,_,_,_,4,_,_,_,9]],
    time(sudoku(S)),
    nl, write('Solución:'), nl,
    imprimir_sudoku_clp(S).

% Ejemplo 3: Verificar si un Sudoku tiene solución única
ejemplo_3 :-
    write('=== EJEMPLO 3: Verificar unicidad ==='), nl,
    S = [[5,3,_,_,7,_,_,_,_],
         [6,_,_,1,9,5,_,_,_],
         [_,9,8,_,_,_,_,6,_],
         [8,_,_,_,6,_,_,_,3],
         [4,_,_,8,_,3,_,_,1],
         [7,_,_,_,2,_,_,_,6],
         [_,6,_,_,_,_,2,8,_],
         [_,_,_,4,1,9,_,_,5],
         [_,_,_,_,8,_,_,7,9]],
    % Encontrar todas las soluciones
    findall(S, sudoku(S), Soluciones),
    length(Soluciones, N),
    write('Número de soluciones: '), write(N), nl,
    (N = 1 -> write('✓ Solución única') ; write('⚠ Múltiples soluciones')), nl.

% Ejemplo 4: Resolver Sudoku vacío (genera un Sudoku válido)
ejemplo_4 :-
    write('=== EJEMPLO 4: Generar Sudoku válido ==='), nl,
    S = [[_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_]],
    write('Generando Sudoku...'), nl,
    sudoku(S),
    imprimir_sudoku_clp(S).

% Ejemplo 5: Validar un Sudoku ya completo
ejemplo_5 :-
    write('=== EJEMPLO 5: Validar Sudoku completo ==='), nl,
    S = [[5,3,4,6,7,8,9,1,2],
         [6,7,2,1,9,5,3,4,8],
         [1,9,8,3,4,2,5,6,7],
         [8,5,9,7,6,1,4,2,3],
         [4,2,6,8,5,3,7,9,1],
         [7,1,3,9,2,4,8,5,6],
         [9,6,1,5,3,7,2,8,4],
         [2,8,7,4,1,9,6,3,5],
         [3,4,5,2,8,6,1,7,9]],
    (sudoku(S) -> write('✓ Sudoku válido') ; write('✗ Sudoku inválido')), nl.

% ----------------------------------------------------------------------------
% EJEMPLOS CON VERSIÓN MANUAL (EDUCATIVA)
% ----------------------------------------------------------------------------

% Cargar la versión manual
:- consult(sudoku_manual).

% Ejemplo 6: Resolver con versión manual (LENTO)
ejemplo_6 :-
    write('=== EJEMPLO 6: Versión manual (puede tardar) ==='), nl,
    S = [5,3,x,x,7,x,x,x,x,
         6,x,x,1,9,5,x,x,x,
         x,9,8,x,x,x,x,6,x,
         8,x,x,x,6,x,x,x,3,
         4,x,x,8,x,3,x,x,1,
         7,x,x,x,2,x,x,x,6,
         x,6,x,x,x,x,2,8,x,
         x,x,x,4,1,9,x,x,5,
         x,x,x,x,8,x,x,7,9],
    write('Resolviendo (espere...)'), nl,
    time(sudoku(S, Sol)),
    write('Solución:'), nl,
    imprimir_sudoku_manual(Sol).

% Ejemplo 7: Comparar ambas versiones
ejemplo_7 :-
    write('=== EJEMPLO 7: Comparación de rendimiento ==='), nl,
    
    % Versión CLP
    S_clp = [[5,3,_,_,7,_,_,_,_],
             [6,_,_,1,9,5,_,_,_],
             [_,9,8,_,_,_,_,6,_],
             [8,_,_,_,6,_,_,_,3],
             [4,_,_,8,_,3,_,_,1],
             [7,_,_,_,2,_,_,_,6],
             [_,6,_,_,_,_,2,8,_],
             [_,_,_,4,1,9,_,_,5],
             [_,_,_,_,8,_,_,7,9]],
    
    write('>>> CLP(FD):'), nl,
    statistics(walltime, [T1|_]),
    sudoku(S_clp),
    statistics(walltime, [T2|_]),
    Tiempo_CLP is T2 - T1,
    write('Tiempo: '), write(Tiempo_CLP), write(' ms'), nl, nl,
    
    % Versión Manual
    S_manual = [5,3,x,x,7,x,x,x,x,
                6,x,x,1,9,5,x,x,x,
                x,9,8,x,x,x,x,6,x,
                8,x,x,x,6,x,x,x,3,
                4,x,x,8,x,3,x,x,1,
                7,x,x,x,2,x,x,x,6,
                x,6,x,x,x,x,2,8,x,
                x,x,x,4,1,9,x,x,5,
                x,x,x,x,8,x,x,7,9],
    
    write('>>> Versión Manual:'), nl,
    statistics(walltime, [T3|_]),
    sudoku(S_manual, _),
    statistics(walltime, [T4|_]),
    Tiempo_Manual is T4 - T3,
    write('Tiempo: '), write(Tiempo_Manual), write(' ms'), nl, nl,
    
    Factor is Tiempo_Manual / Tiempo_CLP,
    write('========================================'), nl,
    write('CLP(FD) es '), write(Factor), write('x más rápido'), nl,
    write('========================================'), nl.

% ----------------------------------------------------------------------------
% EJEMPLOS INTERACTIVOS
% ----------------------------------------------------------------------------

% Ejemplo 8: Resolver paso a paso (modo educativo)
ejemplo_8 :-
    write('=== EJEMPLO 8: Modo paso a paso ==='), nl,
    write('Este ejemplo muestra cómo Prolog busca la solución'), nl, nl,
    
    S = [[5,3,_,_,7,_,_,_,_],
         [6,_,_,1,9,5,_,_,_],
         [_,9,8,_,_,_,_,6,_],
         [8,_,_,_,6,_,_,_,3],
         [4,_,_,8,_,3,_,_,1],
         [7,_,_,_,2,_,_,_,6],
         [_,6,_,_,_,_,2,8,_],
         [_,_,_,4,1,9,_,_,5],
         [_,_,_,_,8,_,_,7,9]],
    
    write('Puzzle inicial:'), nl,
    imprimir_sudoku_clp(S), nl,
    
    write('Resolviendo con CLP(FD)...'), nl,
    trace,  % Activa el debugger para ver paso a paso
    sudoku(S),
    notrace,  % Desactiva el debugger
    
    nl, write('Solución encontrada:'), nl,
    imprimir_sudoku_clp(S).

% ----------------------------------------------------------------------------
% CASOS ESPECIALES
% ----------------------------------------------------------------------------

% Ejemplo 9: Sudoku sin solución (inválido)
ejemplo_9 :-
    write('=== EJEMPLO 9: Sudoku inválido ==='), nl,
    S = [[1,1,_,_,_,_,_,_,_],  % Dos 1s en la primera fila
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_],
         [_,_,_,_,_,_,_,_,_]],
    write('Intentando resolver...'), nl,
    (sudoku(S) -> write('Resuelto') ; write('✗ No tiene solución (como esperábamos)')), nl.

% Ejemplo 10: Sudoku con múltiples soluciones
ejemplo_10 :-
    write('=== EJEMPLO 10: Múltiples soluciones ==='), nl, nl,

    % Uso de variables nombradas para las celdas vacías
    S = [[1, A2, A3, A4, A5, 4, A7, 5, A9],
         [B1,B2,B3,B4,B5,B6,B7,B8,B9],
         [C1,C2,C3,C4,C5,C6,C7,C8,C9],
         [D1,D2,D3,D4,D5,D6,D7,D8,D9],
         [E1,E2,E3,E4,E5,E6,E7,E8,E9],
         [F1,F2,F3,F4,F5,F6,F7,F8,F9],
         [G1,G2,G3,G4,G5,G6,G7,G8,G9],
         [H1,H2,H3,H4,H5,H6,H7,H8,H9],
         [I1,I2,I3,I4,I5,I6,I7,I8,I9]],


    write('Mostrando primeras 3 soluciones:'), nl,
    write('----------------------------------------'), nl,

    findnsols(3, S, sudoku(S), Soluciones),
    imprimir_soluciones(Soluciones, 1).


    imprimir_soluciones([], _).
imprimir_soluciones([S|Resto], N) :-
    format('Solución ~d~n', [N]),
    write('----------------------------------------'), nl,
    imprimir_sudoku_clp(S),
    nl,
    N1 is N + 1,
    imprimir_soluciones(Resto, N1).
% ----------------------------------------------------------------------------
% MENÚ DE AYUDA
% ----------------------------------------------------------------------------

ayuda :-
    nl,
    write('========================================'), nl,
    write('  EJEMPLOS DISPONIBLES'), nl,
    write('========================================'), nl,
    write('ejemplo_1.  - Sudoku simple'), nl,
    write('ejemplo_2.  - Medición de tiempo'), nl,
    write('ejemplo_3.  - Verificar solución única'), nl,
    write('ejemplo_4.  - Generar Sudoku válido'), nl,
    write('ejemplo_5.  - Validar Sudoku completo'), nl,
    write('ejemplo_6.  - Versión manual (lento)'), nl,
    write('ejemplo_7.  - Comparar rendimiento'), nl,
    write('ejemplo_8.  - Modo paso a paso'), nl,
    write('ejemplo_9.  - Sudoku inválido'), nl,
    write('ejemplo_10. - Múltiples soluciones'), nl,
    write('========================================'), nl,
    write('ayuda.      - Mostrar esta ayuda'), nl,
    write('========================================'), nl, nl.

% Mostrar ayuda al cargar
:- ayuda.