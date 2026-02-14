% ============================================================================
% SUDOKU SOLVER CON CLP(FD) - Constraint Logic Programming over Finite Domains
% ============================================================================
% Autores: [Tu nombre] y [Compañero]
% Fecha: Febrero 2026
% Materia: Programación Lógica y Funcional
% Descripción:
%   Solucionador de Sudoku usando programación lógica con restricciones.
%   Este enfoque es declarativo: solo especificamos QUÉ debe cumplirse,
%   y el motor CLP(FD) se encarga de CÓMO encontrar la solución.
% Ventajas de CLP(FD):
%   - Propagación automática de restricciones
%   - Poda inteligente del espacio de búsqueda
%   - Código conciso y legible
%   - Performance excelente
% ============================================================================

:- use_module(library(clpfd)).

% ----------------------------------------------------------------------------
% sudoku(?Filas)
%
% Predicado principal que resuelve un Sudoku representado como lista de listas.
%
% @param Filas: Matriz 9x9 representada como lista de 9 listas
%               Cada celda puede ser un número (1-9) o una variable libre (_)
%
% Ejemplo de uso:
%   ?- sudoku([[_,_,_,_,_,_,_,_,_],
%              [_,_,_,_,_,3,_,8,5],
%              [_,_,1,_,2,_,_,_,_],
%              [_,_,_,5,_,7,_,_,_],
%              [_,_,4,_,_,_,1,_,_],
%              [_,9,_,_,_,_,_,_,_],
%              [5,_,_,_,_,_,_,7,3],
%              [_,_,2,_,1,_,_,_,_],
%              [_,_,_,_,4,_,_,_,9]], Solution).

% sudoku(+Filas)
%
% Resuelve un Sudoku representado como matriz 9x9.
% Cada celda puede ser:
%   - Un entero 1..9 (valor fijo)
%   - 0              (celda vacía)
%   - Una variable libre (_)
% ----------------------------------------------------------------------------
sudoku(Filas) :-
    % 1. Aplanar la matriz en una lista de 81 elementos
    append(Filas, Vs),
    
    % 2. Normalizar celdas:
    %    - 0 → variable libre
    %    - variables y números quedan intactos
    Vs ins 1..9,
    
    % 4. RESTRICCIÓN DE FILAS:
    %    Cada fila debe tener todos los números del 1 al 9 (sin repetición)
    maplist(all_distinct, Filas),
    
    % 5. RESTRICCIÓN DE COLUMNAS:
    %    Transponemos la matriz para obtener columnas como filas
    %    Luego aplicamos all_distinct a cada columna
    transpose(Filas, Columnas),
    maplist(all_distinct, Columnas),
    
    % 6. RESTRICCIÓN DE BLOQUES 3x3:
    %    Extraemos los 9 bloques y verificamos que sean distintos
    bloques(Filas, Bloques),
    maplist(all_distinct, Bloques),
    
    % 7. SOLUTION SEARCH (LABELLING)
    %    - ff (first fail): elige primero la variable con menos opciones
    %    - Maximiza la poda del espacio de búsqueda
    %    - La propagación de restricciones sigue activa en todo momento
    labeling([ff], Vs).

    % labeling([], Vs).        % Default, menos eficiente
    % labeling([ffc], Vs).     % Variante más sofisticada
    % labeling([bisect], Vs).  % Útil para dominios grandes 
    % TODO se podria fijar para hacer un benchmark con distintas heuristicas



% ----------------------------------------------------------------------------
% bloques(+Filas, -Bloques)
%
% Extrae los 9 bloques de 3x3 de un tablero de Sudoku.
%
% Estrategia:
%   - Divide las filas en grupos de 3 (filas 1-3, 4-6, 7-9)
%   - Para cada grupo de 3 filas, extrae los 3 bloques horizontales
%   - Combina todos los bloques en una sola lista
%
% @param Filas: Matriz 9x9 del sudoku
% @param Bloques: Lista de 9 bloques, cada uno con 9 elementos
% ----------------------------------------------------------------------------
bloques([], []).
bloques([F1,F2,F3|Filas], Bloques) :-
    % Procesar 3 filas a la vez
    bloques_de_tres_filas(F1, F2, F3, BloquesSup),
    % Recursión con las siguientes 3 filas
    bloques(Filas, BloquesResto),
    % Combinar bloques
    append(BloquesSup, BloquesResto, Bloques).

% ----------------------------------------------------------------------------
% bloques_de_tres_filas(+F1, +F2, +F3, -Bloques)
%
% Extrae 3 bloques de 3 filas consecutivas.
%
% Ejemplo visual:
%   F1: [1,2,3|4,5,6|7,8,9]
%   F2: [4,5,6|7,8,9|1,2,3]  -->  B1: [1,2,3,4,5,6,7,8,9]
%   F3: [7,8,9|1,2,3|4,5,6]       B2: [4,5,6,7,8,9,1,2,3]
%                                  B3: [7,8,9,1,2,3,4,5,6]
% ----------------------------------------------------------------------------
bloques_de_tres_filas([], [], [], []).
bloques_de_tres_filas([A,B,C|F1], [D,E,F|F2], [G,H,I|F3], 
                      [[A,B,C,D,E,F,G,H,I]|Bloques]) :-
    bloques_de_tres_filas(F1, F2, F3, Bloques).

% ============================================================================
% PREDICADOS DE UTILIDAD
% ============================================================================

% ----------------------------------------------------------------------------
% imprimir_sudoku_clp(+Filas)
%
% Imprime el Sudoku de forma legible en la consola.
% Útil para visualizar la solución.
%
% Ejemplo:
%   ?- sudoku(S), imprimir_sudoku_clp(S).
% ----------------------------------------------------------------------------
imprimir_sudoku_clp([]).
imprimir_sudoku_clp([Fila|Resto]) :-
    imprimir_fila_clp(Fila),
    imprimir_sudoku_clp(Resto).

imprimir_fila_clp([]) :- nl.
imprimir_fila_clp([X|Xs]) :-
    write(X), write(' '),
    imprimir_fila_clp(Xs).

% ----------------------------------------------------------------------------
% resolver_e_imprimir(+Sudoku)
%
% Resuelve un Sudoku y lo imprime de forma bonita.
% Combina sudoku/1 e imprimir_sudoku_clp/1 para mayor comodidad.
%
% Ejemplo:
%   ?- resolver_e_imprimir([[_,_,_,_,_,_,_,_,_],...]).
% ----------------------------------------------------------------------------
resolver_e_imprimir(Sudoku) :-
    sudoku(Sudoku),
    nl,
    write('========== SOLUCION =========='), nl,
    imprimir_sudoku_clp(Sudoku),
    write('=============================='), nl.

% ============================================================================
% EJEMPLOS PREDEFINIDOS
% ============================================================================

% Sudoku fácil
ejemplo_facil([
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

% Sudoku medio
ejemplo_medio([
    [_,_,_,_,_,_,_,_,_],
    [_,_,_,_,_,3,_,8,5],
    [_,_,1,_,2,_,_,_,_],
    [_,_,_,5,_,7,_,_,_],
    [_,_,4,_,_,_,1,_,_],
    [_,9,_,_,_,_,_,_,_],
    [5,_,_,_,_,_,_,7,3],
    [_,_,2,_,1,_,_,_,_],
    [_,_,_,_,4,_,_,_,9]
]).

% Sudoku difícil (World's Hardest Sudoku)
ejemplo_dificil([
    [8,_,_,_,_,_,_,_,_],
    [_,_,3,6,_,_,_,_,_],
    [_,7,_,_,9,_,2,_,_],
    [_,5,_,_,_,7,_,_,_],
    [_,_,_,_,4,5,7,_,_],
    [_,_,_,1,_,_,_,3,_],
    [_,_,1,_,_,_,_,6,8],
    [_,_,8,5,_,_,_,1,_],
    [_,9,_,_,_,_,4,_,_]
]).

% Sudoku extremo (AI Escargot - 2006)
ejemplo_extremo([
    [1,_,_,_,_,7,_,9,_],
    [_,3,_,_,2,_,_,_,8],
    [_,_,9,6,_,_,5,_,_],
    [_,_,5,3,_,_,9,_,_],
    [_,1,_,_,8,_,_,_,2],
    [6,_,_,_,_,4,_,_,_],
    [3,_,_,_,_,_,_,1,_],
    [_,4,_,_,_,_,_,_,7],
    [_,_,7,_,_,_,3,_,_]
]).

% Sudoku del documento original (válido y con solución única)
ejemplo_documento([
    [4,_,_,_,6,_,9,1,_],
    [2,_,_,_,_,7,_,5,_],
    [_,9,_,8,_,_,_,2,_],
    [_,_,1,6,_,9,_,_,2],
    [_,8,_,_,_,_,_,6,3],
    [_,7,_,_,4,_,_,_,_],
    [7,_,3,_,_,8,_,9,_],
    [_,_,_,_,3,_,4,_,5],
    [_,4,_,9,_,_,6,_,_]
]).

% Ejemplos de uso desde la consola:
%
% FÁCIL (segundos):
% ?- ejemplo_facil(S), resolver_e_imprimir(S).
% ?- ejemplo_medio(S), resolver_e_imprimir(S).
%
% DIFÍCIL (puede tardar más):
% ?- ejemplo_dificil(S), resolver_e_imprimir(S).
% ?- ejemplo_extremo(S), resolver_e_imprimir(S).
% ?- ejemplo_documento(S), resolver_e_imprimir(S).
%
% Para medir el tiempo de ejecución:
% ?- ejemplo_medio(S), time(sudoku(S)).
% ?- ejemplo_extremo(S), time(sudoku(S)).
%
% NOTA: ejemplo_facil y ejemplo_medio son bastante rápidos.
%       ejemplo_dificil y ejemplo_extremo pueden tardar varios segundos.
%       Con labeling([ff], Vs) la resolución es mucho más rápida.