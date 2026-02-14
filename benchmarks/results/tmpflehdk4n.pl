:- use_module(library(lists)).
:- consult('C:/Users/Ericsen/OneDrive/Desktop/sudoku-project/sudoku-comparative-study/prolog/src/sudoku_manual.pl').
:- initialization(main, main).
main :-
    Input = [x,x,2,4,x,x,8,x,x,6,x,x,x,x,x,x,5,x,x,x,x,x,5,x,x,x,6,x,8,x,9,x,x,6,x,x,x,x,x,x,6,x,x,x,x,x,x,7,x,x,3,x,8,x,5,x,x,x,8,x,x,x,x,x,3,x,x,x,x,x,x,9,x,x,1,x,x,7,4,x,x],
    get_time(T1),
    ( sudoku(Input, _) -> true ; true ),
    get_time(T2),
    Elapsed is (T2 - T1) * 1000.0,
    format('~6f~n', [Elapsed]),
    halt.
