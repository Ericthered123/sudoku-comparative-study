:- use_module(library(lists)).
:- consult('C:/Users/Ericsen/OneDrive/Desktop/sudoku-project/sudoku-comparative-study/prolog/src/sudoku_manual.pl').
:- initialization(main, main).
main :-
    Input = [x,x,2,4,x,x,8,x,x,6,x,x,x,x,x,x,5,x,x,x,x,x,5,x,x,x,6,x,8,x,9,x,x,6,x,x,x,x,x,x,6,x,x,x,x,x,x,7,x,x,3,x,8,x,5,x,x,x,8,x,x,x,x,x,3,x,x,x,x,x,x,9,x,x,1,x,x,7,4,x,x],
    statistics(walltime, [_, _]),
    (sudoku(Input, _) -> true ; true),
    statistics(walltime, [_, T]),
    format('~w~n', [T]),
    halt.
