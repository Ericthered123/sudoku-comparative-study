% ============================================================================
% SUDOKU SOLVER MANUAL - Sin usar CLP(FD)
% ============================================================================
% Autores: [Eric Doyle] y [Bruno Lodeiro]
% Fecha: Febrero 2026
% Materia: Programación Lógica y Funcional
%
% Descripción:
%   Implementación educativa del solucionador de Sudoku usando solo
%   backtracking explícito sin bibliotecas de restricciones.
%
% Propósito:
%   Mostrar cómo funciona el backtracking internamente y entender
%   la diferencia con el enfoque declarativo de CLP(FD).
%
% ADVERTENCIA: Esta implementación es LENTA comparada con CLP(FD).
%              Solo se usa con fines didácticos.
%
% Representación:
%   Lista plana de 81 elementos, donde:
%   - Los números (1-9) son valores fijos del puzzle
%   - La variable 'x' representa celdas vacías a resolver
% ============================================================================

% ----------------------------------------------------------------------------
% EXTRACCIÓN DE FILAS, COLUMNAS Y CUADRADOS
% ----------------------------------------------------------------------------

% row(+Sudoku, +N, -Row)
%
% Extrae la fila N del tablero (N va de 1 a 9).
% Calcula los índices de los 9 elementos de la fila usando aritmética.
%
% Fórmula: Para la fila N, los índices son: (N-1)*9+1 hasta (N-1)*9+9
%
% Ejemplo: Fila 1 → índices 1,2,3,4,5,6,7,8,9
%          Fila 2 → índices 10,11,12,13,14,15,16,17,18
row(Sudoku, N, Row) :-
    N0 is N-1,
    N1 is N0*9+1, nth1(N1, Sudoku, X1),
    N2 is N0*9+2, nth1(N2, Sudoku, X2),
    N3 is N0*9+3, nth1(N3, Sudoku, X3),
    N4 is N0*9+4, nth1(N4, Sudoku, X4),
    N5 is N0*9+5, nth1(N5, Sudoku, X5),
    N6 is N0*9+6, nth1(N6, Sudoku, X6),
    N7 is N0*9+7, nth1(N7, Sudoku, X7),
    N8 is N0*9+8, nth1(N8, Sudoku, X8),
    N9 is N0*9+9, nth1(N9, Sudoku, X9),
    Row = [X1, X2, X3, X4, X5, X6, X7, X8, X9].

% column(+Sudoku, +N, -Column)
%
% Extrae la columna N del tablero (N va de 1 a 9).
% Las columnas se toman cada 9 posiciones.
%
% Fórmula: Para columna N, los índices son: N, 9+N, 18+N, 27+N, ...
%
% Ejemplo: Columna 1 → índices 1,10,19,28,37,46,55,64,73
%          Columna 5 → índices 5,14,23,32,41,50,59,68,77
column(Sudoku, N, Column) :-
    N1 is 0*9+N, nth1(N1, Sudoku, X1),
    N2 is 1*9+N, nth1(N2, Sudoku, X2),
    N3 is 2*9+N, nth1(N3, Sudoku, X3),
    N4 is 3*9+N, nth1(N4, Sudoku, X4),
    N5 is 4*9+N, nth1(N5, Sudoku, X5),
    N6 is 5*9+N, nth1(N6, Sudoku, X6),
    N7 is 6*9+N, nth1(N7, Sudoku, X7),
    N8 is 7*9+N, nth1(N8, Sudoku, X8),
    N9 is 8*9+N, nth1(N9, Sudoku, X9),
    Column = [X1, X2, X3, X4, X5, X6, X7, X8, X9].

% square(+Sudoku, +N, -Square)
%
% Extrae el cuadrado (bloque 3x3) número N del tablero (N va de 1 a 9).
%
% Los bloques se numeran así:
%   1 2 3
%   4 5 6
%   7 8 9
%
% Fórmula compleja usando división y módulo para calcular offset:
%   O = (N-1) // 3  → Fila de bloques (0,1,2)
%   P = (N-1) mod 3 → Columna de bloques (0,1,2)
%
% Cada bloque tiene índices siguiendo patrón:
%   O*27 + P*3 + {1,2,3,10,11,12,19,20,21}
square(Sudoku, N, Square) :-
    O is (N-1) // 3,
    P is (N-1) mod 3,
    N1 is O*27+P*3+1,  nth1(N1, Sudoku, X1),
    N2 is O*27+P*3+2,  nth1(N2, Sudoku, X2),
    N3 is O*27+P*3+3,  nth1(N3, Sudoku, X3),
    N4 is O*27+P*3+10, nth1(N4, Sudoku, X4),
    N5 is O*27+P*3+11, nth1(N5, Sudoku, X5),
    N6 is O*27+P*3+12, nth1(N6, Sudoku, X6),
    N7 is O*27+P*3+19, nth1(N7, Sudoku, X7),
    N8 is O*27+P*3+20, nth1(N8, Sudoku, X8),
    N9 is O*27+P*3+21, nth1(N9, Sudoku, X9),
    Square = [X1, X2, X3, X4, X5, X6, X7, X8, X9].

% ----------------------------------------------------------------------------
% VALIDACIÓN
% ----------------------------------------------------------------------------

% valid(+R)
%
% Verifica que una lista (fila/columna/cuadrado) sea válida.
% Una lista es válida si es un SET (no tiene elementos duplicados).
%
% NOTA: is_set/1 es un predicado built-in de SWI-Prolog que verifica
%       que no haya duplicados en una lista.
%
% Esta validación NO añade números, solo verifica.
% Los números se añaden en el predicado check/2 usando between/3.
valid(R) :-
    is_set(R).

% ----------------------------------------------------------------------------
% DCG (Definite Clause Grammars) PARA TRANSFORMAR ENTRADA
% ----------------------------------------------------------------------------

% program//1
%
% DCG que transforma la lista de entrada.
% - Números fijos permanecen como están
% - Variables 'x' se transforman en variables lógicas de Prolog (_)
%
% Esto permite que el motor de Prolog unifique y asigne valores.
program([H|T]) --> digit(H), program(T).
program([]) --> [].

% digit//1
%
% Regla DCG que procesa cada elemento:
% - Si es un número, lo mantiene
% - Si es 'x', lo convierte en variable libre
digit(N) --> [N], { number(N) }.
digit(_) --> [x].

% ----------------------------------------------------------------------------
% PREDICADO PRINCIPAL DE RESOLUCIÓN
% ----------------------------------------------------------------------------

% sudoku(+Sudoku, -SolvedSudoku)
%
% Resuelve el Sudoku usando backtracking explícito.
%
% Proceso:
%   1. phrase/2 aplica el DCG para transformar x → variables
%   2. El cut (!) evita backtracking innecesario después de la transformación
%   3. maplist/2 itera sobre cada celda aplicando check/2
%   4. check/2 asigna valores (1-9) y valida todas las restricciones
%
% @param Sudoku: Lista de 81 elementos con números y 'x'
% @param SolvedSudoku: Lista de 81 números (solución)
%
% MECANISMO DE BACKTRACKING:
%   - Si check/2 falla para alguna celda, Prolog retrocede
%   - Prueba otro valor para la celda anterior
%   - Si ningún valor funciona, retrocede más atrás
%   - Continúa hasta encontrar solución o agotar posibilidades
sudoku(Sudoku, SolvedSudoku) :-
    phrase(program(SolvedSudoku), Sudoku), !,
    maplist(check(SolvedSudoku), SolvedSudoku).

% ----------------------------------------------------------------------------
% VERIFICACIÓN Y GENERACIÓN DE VALORES
% ----------------------------------------------------------------------------

% check(+SolvedSudoku, ?N)
%
% Para cada celda N del sudoku:
%   - Si N ya tiene valor (número fijo), solo valida restricciones
%   - Si N es variable, between/3 genera valores 1-9 secuencialmente
%   - Para cada valor, verifica TODAS las restricciones
%   - Si alguna restricción falla, backtracking prueba siguiente valor
%
% RESTRICCIONES VERIFICADAS:
%   - Todas las 9 filas deben ser válidas (no repetir números)
%   - Todas las 9 columnas deben ser válidas
%   - Todos los 9 cuadrados 3x3 deben ser válidos
%
% ADVERTENCIA: Este enfoque es EXTREMADAMENTE INEFICIENTE porque:
%   - Valida TODAS las restricciones para CADA celda
%   - No hay propagación de restricciones
%   - Backtracking "ciego" sin poda inteligente
%   - Complejidad exponencial en el peor caso
check(SolvedSudoku, N) :-
    % Generar/verificar que N esté entre 1 y 9
    between(1, 9, N),
    
    % Validar todas las filas
    row(SolvedSudoku, 1, R1), valid(R1),
    row(SolvedSudoku, 2, R2), valid(R2),
    row(SolvedSudoku, 3, R3), valid(R3),
    row(SolvedSudoku, 4, R4), valid(R4),
    row(SolvedSudoku, 5, R5), valid(R5),
    row(SolvedSudoku, 6, R6), valid(R6),
    row(SolvedSudoku, 7, R7), valid(R7),
    row(SolvedSudoku, 8, R8), valid(R8),
    row(SolvedSudoku, 9, R9), valid(R9),
    
    % Validar todas las columnas
    column(SolvedSudoku, 1, C1), valid(C1),
    column(SolvedSudoku, 2, C2), valid(C2),
    column(SolvedSudoku, 3, C3), valid(C3),
    column(SolvedSudoku, 4, C4), valid(C4),
    column(SolvedSudoku, 5, C5), valid(C5),
    column(SolvedSudoku, 6, C6), valid(C6),
    column(SolvedSudoku, 7, C7), valid(C7),
    column(SolvedSudoku, 8, C8), valid(C8),
    column(SolvedSudoku, 9, C9), valid(C9),
    
    % Validar todos los cuadrados 3x3
    square(SolvedSudoku, 1, S1), valid(S1),
    square(SolvedSudoku, 2, S2), valid(S2),
    square(SolvedSudoku, 3, S3), valid(S3),
    square(SolvedSudoku, 4, S4), valid(S4),
    square(SolvedSudoku, 5, S5), valid(S5),
    square(SolvedSudoku, 6, S6), valid(S6),
    square(SolvedSudoku, 7, S7), valid(S7),
    square(SolvedSudoku, 8, S8), valid(S8),
    square(SolvedSudoku, 9, S9), valid(S9).

% ============================================================================
% UTILIDADES PARA VISUALIZACIÓN
% ============================================================================

% imprimir_sudoku_manual(+Lista)
%
% Imprime el sudoku en formato legible 9x9.
imprimir_sudoku_manual(Lista) :-
    imprimir_filas_manual(Lista, 1).

imprimir_filas_manual(_, 10) :- !.
imprimir_filas_manual(Lista, N) :-
    row(Lista, N, Fila),
    imprimir_fila_manual(Fila),
    N1 is N + 1,
    imprimir_filas_manual(Lista, N1).

imprimir_fila_manual([]) :- nl.
imprimir_fila_manual([X|Xs]) :-
    (number(X) -> write(X) ; write('_')),
    write(' '),
    imprimir_fila_manual(Xs).

% ============================================================================
% EJEMPLOS DE USO
% ============================================================================

% Sudoku de ejemplo (del documento original)
ejemplo([
    4,x,x,x,6,x,9,1,x,
    2,x,x,x,x,7,x,5,x,
    x,9,x,8,x,x,x,2,x,
    x,x,1,6,x,9,x,x,2,
    x,8,x,x,x,x,x,6,3,
    x,7,x,x,4,x,x,x,x,
    7,x,3,x,x,8,x,9,x,
    x,x,x,x,3,x,4,x,5,
    x,4,x,9,x,x,6,x,x
]).

% Sudoku más fácil para pruebas rápidas
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

ejemplo_maligno1([
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

% Ejemplos de uso:
%
% ?- ejemplo_simple(S), sudoku(S, Sol), imprimir_sudoku_manual(Sol).
% ?- ejemplo(S), time(sudoku(S, Sol)).
%
% NOTA: ejemplo/1 puede tardar MUCHO tiempo (minutos).
%       Use ejemplo_simple/1 para pruebas rápidas.