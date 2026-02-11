% ============================================================================
% SUDOKU SOLVER MANUAL + MRV explicito - Sin usar CLP(FD)
% ============================================================================
% Authors: [Eric Doyle] and [Bruno Lodeiro]
% Fecha: Febrero 2026
% Materia: Programación Lógica y Funcional


:- use_module(library(lists)).

% ============================================================================
% FILAS / COLUMNAS / CUADRADOS
% - Row/3, Column/3 y Square/3 extraen las estructuras del tablero plano (81).
% - Board es una lista plana de 81 elementos.
% ============================================================================

% row(Board, N, Row)
%   Row = lista de 9 elementos de la fila N (N = 1..9)
row(Board, N, Row) :-
    Start is (N-1)*9+1, End is N*9,
    findall(X, (between(Start,End,I), nth1(I,Board,X)), Row).


% column(Board, N, Col)
%   Col = lista de 9 elementos de la columna N (N = 1..9)
column(Board, N, Col) :-
    findall(X, (between(0,8,K), I is K*9+N, nth1(I,Board,X)), Col).


% square(Board, N, Sq)
%   Sq = lista de 9 elementos del bloque 3x3 N (N = 1..9)
square(Board, N, Sq) :-
    RowStart is ((N-1)//3)*3 +1,
    ColStart is ((N-1) mod 3)*3 +1,
    findall(X, (between(0,2,R), between(0,2,C),
        I is (RowStart+R-1)*9 + (ColStart+C),
%              [_,_,_,_,4,_,_,_,9]], Solution).


% ============================================================================
% POSICIONES de celdas (índice plano -> fila/col/cuadro)
% ============================================================================

pos_to_row(Pos,R) :- R is ((Pos-1)//9)+1.
pos_to_col(Pos,C) :- C is ((Pos-1) mod 9)+1.
pos_to_square(Pos,S) :-
    pos_to_row(Pos,R), pos_to_col(Pos,C),
    S is ((R-1)//3)*3 + ((C-1)//3) +1.

% ============================================================================
% PREPROCESS: convertir 'x' de la entrada en variables reales (VarsBoard)
% - InputBoard: lista con números y átomos 'x'
% - VarsBoard: nueva lista donde cada 'x' fue reemplazado por una variable Prolog
% ============================================================================
preprocess([], []).
preprocess([x|Xs], [V|Ys]) :- V = _, preprocess(Xs,Ys).  % x -> nueva variable
preprocess([N|Xs], [N|Ys]) :- N \= x, preprocess(Xs,Ys).

% taken_in(+List, +V)
%   true si V aparece en List como elemento *fijo* (nonvar), sin tocar variables
taken_in(List, V) :-
    member(E, List),
    nonvar(E),
    E == V.

% remove_taken(+List, +CandidatesIn, -CandidatesOut)
%   Quita de CandidatesIn los valores que aparecen en List como nonvar.
remove_taken(_, [], []).
remove_taken(List, [C|Cs], Out) :-
    ( taken_in(List, C) ->
        remove_taken(List, Cs, Out)
    ;
        Out = [C|OutTail],
        remove_taken(List, Cs, OutTail)
    ).

% ============================================================================
% CÁLCULO DE CANDIDATOS (sin unificar variables)
% - importante: **no** queremos que 'member' unifique variables de la fila/col/cuadrado.
% - remove_taken filtra candidatos basándose sólo en elementos nonvar (valores fijos).
% ============================================================================
candidates(Board, Pos, Cands) :-
    findall(N, between(1,9,N), All),
    pos_to_row(Pos,R), row(Board,R,Row), remove_taken(Row, All, R1),
    pos_to_col(Pos,C), column(Board,C,Col), remove_taken(Col, R1, R2),
    pos_to_square(Pos,S), square(Board,S,Square), remove_taken(Square, R2, Cands).

% ============================================================================
% MRV (Most Constrained Variable) + backtracking puro
% - select_mrv elige la celda variable con menor número de candidatos > 0
% - solve_mrv prueba candidatos y deja que Prolog haga backtracking naturalmente
% ============================================================================
solve_mrv(Board) :- 
    select_mrv(Board, Pos, Cands), % selecciona celda más restringida
    !,
    nth1(Pos, Board, Var),
    member(Value, Cands), % prueba cada candidato
    Var = Value,          % asigna y continúa (backtracking si falla)
    solve_mrv(Board). 
solve_mrv(_). % caso base: no quedan variables -> resuelto


% select_mrv(Board, Pos, Cands)
%   Construye lista [Len-Pos-Cands] y toma la mínima por Len (MRV)
select_mrv(Board, Pos, Cands) :-
    findall(Len-P-Cs,
        ( nth1(P,Board,Val), var(Val),  % sólo variables libres
          candidates(Board,P,Cs),
          length(Cs,Len), Len>0),
        L),
    L \= [],                            % si lista vacía, no hay variables
    keysort(L, [_-Pos-Cands|_]).

% ============================================================================
% API usable
% - resolver(InputBoard, Solved) deja la solución en Solved (lista resuelta)
% - resolver_e_imprimir(InputBoard) imprime la solución
% ============================================================================
resolver(InputBoard, Solved) :-
    preprocess(InputBoard, VarsBoard),
    solve_mrv(VarsBoard),
    Solved = VarsBoard.

resolver_e_imprimir(InputBoard) :-
    resolver(InputBoard, Solved),
    nl, write('--- Sudoku resuelto ---'), nl,
    imprimir(Solved).

% ============================================================================
% IMPRESIÓN (formato 9x9)
% ============================================================================
imprimir(Board) :- imprimir_filas(Board,1).

imprimir_filas(_,10) :- !.
imprimir_filas(Board,N) :-
    row(Board,N,F),
    imprimir_fila(F),
    N1 is N+1,
    imprimir_filas(Board,N1).

imprimir_fila([]) :- nl.
imprimir_fila([X|Xs]) :- (var(X) -> write('_') ; write(X)), write(' '), imprimir_fila(Xs).

% -------------------------------------------------------------------------
% EJEMPLOS
% -------------------------------------------------------------------------
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
    x,x,x,x,x,x,x,x,x,
    x,x,x,x,x,3,x,8,5,
    x,x,1,x,2,x,x,x,x,
    x,x,x,5,x,7,x,x,x,
    x,x,4,x,x,x,1,x,x,
    x,9,x,x,x,x,x,x,x,
    5,x,x,x,x,x,x,7,3,
    x,x,2,x,1,x,x,x,x,
    x,x,x,x,4,x,x,x,9
]).
