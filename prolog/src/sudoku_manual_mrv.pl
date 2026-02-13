% ============================================================================
% SUDOKU SOLVER MANUAL + MRV explicito - Sin usar CLP(FD)
% ============================================================================
% Authors: [Eric Doyle] and [Bruno Lodeiro]
% Fecha: Febrero 2026
% Materia: Programación Lógica y Funcional
%
% Descripción:
%   Solver de Sudoku con backtracking explícito y heurística MRV
%   (Most Constrained Variable / Minimum Remaining Values).
%
%   En lugar de probar celdas en orden secuencial, seleccionamos siempre
%   la celda vacía con menos valores candidatos posibles, lo que reduce
%   drásticamente el factor de ramificación del árbol de búsqueda.
%
% Complejidad comparada:
%   - Naive BT:  O(9^n)  — n = celdas vacías, sin poda
%   - MRV:       O(9^n)  — igual en peor caso, pero poda efectiva en práctica
%   - CLP(FD):   Propagación + labeling ff → mucho más eficiente

:- use_module(library(lists)).

% ============================================================================
% FILAS / COLUMNAS / CUADRADOS
% - Row/3, Column/3 y Square/3 extraen las estructuras del tablero plano (81).
% - Board es una lista plana de 81 elementos.
% ============================================================================

% row(+Board, +N, -Row)
%   Extrae la fila N (1-9) del tablero plano.
%   Fórmula: índices (N-1)*9+1 .. N*9
row(Board, N, Row) :-
    Start is (N-1)*9 + 1,
    End   is N*9,
    findall(X, (between(Start, End, I), nth1(I, Board, X)), Row).

% column(+Board, +N, -Col)
%   Extrae la columna N (1-9).
%   Fórmula: índices N, 9+N, 18+N, ..., 72+N
column(Board, N, Col) :-
    findall(X, (between(0, 8, K), I is K*9 + N, nth1(I, Board, X)), Col).

% square(+Board, +N, -Sq)
%   Extrae el bloque 3×3 número N (1-9).
%   Bloques numerados de izquierda a derecha, arriba a abajo:
%     1 2 3
%     4 5 6
%     7 8 9
%
%   RowStart = ((N-1)//3)*3 + 1   → fila inicial del bloque
%   ColStart = ((N-1) mod 3)*3 + 1 → columna inicial del bloque
square(Board, N, Sq) :-
    RowStart is ((N-1) // 3) * 3 + 1,
    ColStart is ((N-1) mod 3) * 3 + 1,
    findall(X,
            ( between(0, 2, R),
              between(0, 2, C),
              Row is RowStart + R,
              Col is ColStart + C,
              I   is (Row - 1) * 9 + Col,
              nth1(I, Board, X) ),
            Sq).

% ============================================================================
% POSICIÓN → FILA / COLUMNA / CUADRADO
% ============================================================================

pos_to_row(Pos, R) :- R is ((Pos - 1) // 9) + 1.
pos_to_col(Pos, C) :- C is ((Pos - 1) mod 9) + 1.
pos_to_square(Pos, S) :-
    pos_to_row(Pos, R),
    pos_to_col(Pos, C),
    S is ((R - 1) // 3) * 3 + ((C - 1) // 3) + 1.

% ============================================================================
% PREPROCESAMIENTO: 'x' → variables Prolog
% ============================================================================

% preprocess(+Input, -VarsBoard)
%   Reemplaza cada 'x' por una variable fresca; números quedan intactos.
preprocess([], []).
preprocess([x | Xs], [_ | Ys]) :- preprocess(Xs, Ys).
preprocess([N | Xs], [N | Ys]) :- N \= x, preprocess(Xs, Ys).

% ============================================================================
% CANDIDATOS (sin unificar variables ajenas)
% ============================================================================

% taken_in(+List, +V)
%   true si V aparece en List como elemento *fijo* (nonvar).
taken_in(List, V) :-
    member(E, List),
    nonvar(E),
    E == V.

% remove_taken(+List, +CandIn, -CandOut)
%   Quita de CandIn los valores que aparecen en List como nonvar.
remove_taken(_, [], []).
remove_taken(List, [C | Cs], Out) :-
    ( taken_in(List, C)
    -> remove_taken(List, Cs, Out)
    ;  Out = [C | OutTail],
       remove_taken(List, Cs, OutTail)
    ).

% candidates(+Board, +Pos, -Cands)
%   Calcula los valores 1-9 válidos para la celda en posición Pos.
%   Filtra lo que ya está en la fila, columna y cuadrado correspondientes.
candidates(Board, Pos, Cands) :-
    findall(N, between(1, 9, N), All),
    pos_to_row(Pos, R),    row(Board, R, Row),       remove_taken(Row,    All, R1),
    pos_to_col(Pos, C),    column(Board, C, Col),    remove_taken(Col,    R1,  R2),
    pos_to_square(Pos, S), square(Board, S, Square), remove_taken(Square, R2,  Cands).

% ============================================================================
% MRV + BACKTRACKING
% ============================================================================

% solve_mrv(+Board)
%   Resuelve el tablero in-place usando MRV:
%     1. Selecciona la celda variable con menos candidatos (select_mrv/3).
%     2. Prueba cada candidato, asigna y recursa (Prolog hace backtracking).
%     3. Caso base: no quedan variables → tablero resuelto.
solve_mrv(Board) :-
    select_mrv(Board, Pos, Cands),
    !,
    nth1(Pos, Board, Var),
    member(Value, Cands),
    Var = Value,
    solve_mrv(Board).
solve_mrv(_).   % No quedan celdas vacías → éxito

% select_mrv(+Board, -Pos, -Cands)
%   Elige la celda libre con el menor número de candidatos (MRV).
%   Usa keysort/2 sobre [Len-Pos-Cands] para obtener el mínimo eficientemente.
select_mrv(Board, Pos, Cands) :-
    findall(Len-P-Cs,
            ( nth1(P, Board, Val),
              var(Val),
              candidates(Board, P, Cs),
              length(Cs, Len)
            ),
            L),
    L \= [],
    keysort(L, [Len-Pos-Cands | _]),
    Len > 0.

% ============================================================================
% API PÚBLICA
% ============================================================================

% resolver(+InputBoard, -Solved)
%   Punto de entrada principal.
%   InputBoard: lista de 81 elementos (enteros 1-9 o el átomo 'x').
%   Solved:     lista de 81 enteros (solución).
resolver(InputBoard, Solved) :-
    preprocess(InputBoard, VarsBoard),
    solve_mrv(VarsBoard),
    maplist(nonvar, VarsBoard),   % 🔒 Garantiza solución completa
    Solved = VarsBoard.

% resolver_e_imprimir(+InputBoard)
%   Resuelve e imprime el tablero en formato 9×9.
resolver_e_imprimir(InputBoard) :-
    resolver(InputBoard, Solved),
    nl, write('--- Sudoku resuelto (MRV) ---'), nl,
    imprimir(Solved).

% ============================================================================
% IMPRESIÓN
% ============================================================================

imprimir(Board) :- imprimir_filas(Board, 1).

imprimir_filas(_, 10) :- !.
imprimir_filas(Board, N) :-
    row(Board, N, F),
    imprimir_fila(F),
    N1 is N + 1,
    imprimir_filas(Board, N1).

imprimir_fila([]) :- nl.
imprimir_fila([X | Xs]) :-
    ( var(X) -> write('_') ; write(X) ),
    write(' '),
    imprimir_fila(Xs).

% ============================================================================
% EJEMPLOS
% ============================================================================

ejemplo_simple([
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

ejemplo_medio([
    x,x,x,2,6,x,7,x,1,
    6,8,x,x,7,x,x,9,x,
    1,9,x,x,x,4,5,x,x,
    8,2,x,1,x,x,x,4,x,
    x,x,4,6,x,2,9,x,x,
    x,5,x,x,x,3,x,2,8,
    x,x,9,3,x,x,x,7,4,
    x,4,x,x,5,x,x,3,6,
    7,x,3,x,1,8,x,x,x
]).

ejemplo_medio2([ 
     x,x,x,x,x,x,x,x,x, 
     x,x,x,x,x,3,x,8,5, 
     x,x,1,x,2,x,x,x,x,
     x,x,x,5,x,7,x,x,x, 
     x,x,4,x,x,x,1,x,x, 
     x,9,x,x,x,x,x,x,x, 
     5,x,x,x,x,x,x,7,3, 
     x,x,2,x,1,x,x,x,x, 
     x,x,x,x,4,x,x,x,9 ]).

% Uso desde consola:
%   ?- ejemplo_simple(S), resolver_e_imprimir(S).
%   ?- ejemplo_medio(S), time(resolver(S, _)).