{-|
Module      : Solver
Description : Algoritmos para resolver Sudoku usando backtracking
Copyright   : (c) 2026 Eric Doyle
License     : BSD3
Maintainer  : eric6doyle@gmail.com

This module implements multiple Sudoku resolution strategies:
  1. FirstEmpty - Backtracking básico (baseline)
  2. MostConstrained (MRV) - Heurística de variable más restringida
  3. PropagationMRV - Propagation of restrictions + MRV (CLP style (FD))
  
description:    Please see the README on GitHub at <https://github.com/Ericthered123/sudoku-comparative-study/blob/main/haskell/README.md>
Estrategia:
  1. Encontrar la primera celda vacía
  2. Probar valores del 1 al 9
  3. Para cada valor válido, resolver recursivamente
  4. Si se encuentra solución, retornarla
  5. If not, backtrack (test next value)

Pure functional Sudoku solver.
Incluye variantes con:
  - Backtracking básico
  - Heurística MRV (Most Constrained Variable)
  - Propagation of restrictions + MRV (CLP style (FD))

  The PropagationMRV strategy implements:
  - Naked Singles: celdas con un solo candidato
  - Hidden Singles: valores únicos en filas/columnas/bloques
  - Iterative propagation to fixed point
  Esto permite una comparación justa con Prolog CLP(FD).
-}  

module Solver
    ( solve
    , solveWithStrategy
    , SolveStrategy(..)
    , candidates
    , isValidPlacement
    , propagationStats
    ) where

import Data.Array
import Data.List (minimumBy)
import Data.Ord (comparing)
import Types

-- | Resolution strategies available
data SolveStrategy 
    = FirstEmpty        -- ^ Selecciona primera celda vacía (O(n))
    | MostConstrained   -- ^ Selecciona celda con menos candidatos - MRV (O(n²))
    | PropagationMRV    -- ^ Propagación + MRV - simula CLP(FD) (O(n³))
    deriving (Show, Eq)

-- | Solve a Sudoku using default strategy (FirstEmpty)
solve :: Board -> Maybe Board
solve board
    | not (isValid board) = Nothing
    | otherwise           = solveWithStrategy FirstEmpty board

-- | Solve a Sudoku using a specific strategy
solveWithStrategy :: SolveStrategy -> Board -> Maybe Board
solveWithStrategy strategy board =
    case preprocess strategy board of
        Nothing -> Nothing  -- Contradicción detectada durante propagación
        Just preprocessedBoard
            | isSolved preprocessedBoard -> Just preprocessedBoard
            | otherwise ->
                case selectCell strategy preprocessedBoard of
                    Nothing  -> Nothing  -- No hay celdas vacías pero no está resuelto
                    Just pos -> tryValues preprocessedBoard pos (candidates preprocessedBoard pos)
  where
    -- Intenta cada valor candidato para la posición seleccionada
    tryValues :: Board -> Position -> [Value] -> Maybe Board
    tryValues _ _ [] = Nothing
    tryValues b pos (v:vs) =
        let newBoard = b // [(pos, Filled v)]
        in case solveWithStrategy strategy newBoard of
            Just solution -> Just solution
            Nothing       -> tryValues b pos vs

-- ============================================================================
-- SELECCIÓN DE VARIABLE (según estrategia)
-- ============================================================================

-- | Selecciona la siguiente celda vacía a llenar según la estrategia
selectCell :: SolveStrategy -> Board -> Maybe Position
selectCell FirstEmpty      board = findFirstEmpty board
selectCell MostConstrained board = findMostConstrained board
selectCell PropagationMRV  board = findMostConstrained board

-- | Encuentra la primera celda vacía (búsqueda lineal simple)
findFirstEmpty :: Board -> Maybe Position
findFirstEmpty board =
    case [pos | pos <- indices board, board ! pos == Empty] of
        []      -> Nothing
        (p : _) -> Just p

-- | Encuentra la celda vacía con menos candidatos (MRV heuristic)
-- Minimum Remaining Values: minimiza el factor de ramificación
findMostConstrained :: Board -> Maybe Position
findMostConstrained board =
    case emptyCells of
        []    -> Nothing
        cells -> Just $ minimumBy (comparing numCandidates) cells
  where
    emptyCells = [pos | pos <- indices board, board ! pos == Empty]
    numCandidates pos = length (candidates board pos)

-- ============================================================================
-- PROPAGACIÓN DE RESTRICCIONES (estilo CLP(FD))
-- ============================================================================
-- Implementa técnicas avanzadas de propagación:
--   1. Naked Singles: celdas con un solo candidato
--   2. Hidden Singles: valores que solo pueden ir en una celda de una unidad
--   3. Propagación iterativa hasta punto fijo
--
-- Esto simula el comportamiento de CLP(FD) en Prolog, permitiendo
-- una comparación más justa entre paradigmas.
-- ============================================================================

-- | Pre-procesa el tablero según la estrategia
-- Solo PropagationMRV hace propagación; las demás pasan el tablero sin cambios
preprocess :: SolveStrategy -> Board -> Maybe Board
preprocess PropagationMRV board = propagate board
preprocess _              board = Just board

-- | Propaga restricciones hasta alcanzar punto fijo
-- Retorna Nothing si se detecta una contradicción (dominio vacío)
propagate :: Board -> Maybe Board
propagate board
    | hasEmptyDomain board = Nothing      -- Contradicción: celda sin candidatos
    | null allSingles      = Just board   -- Punto fijo alcanzado
    | otherwise            = propagate (fillCells board allSingles)
  where
    allSingles = nakedSingles board ++ hiddenSingles board

-- | Verifica si alguna celda vacía no tiene candidatos válidos
-- Esto indica una contradicción en el estado actual
hasEmptyDomain :: Board -> Bool
hasEmptyDomain board =
    any (\pos -> board ! pos == Empty && null (candidates board pos))
        (indices board)

-- | Llena múltiples celdas en el tablero de forma pura (sin mutación)
fillCells :: Board -> [(Position, Value)] -> Board
fillCells board singles = board // [(pos, Filled val) | (pos, val) <- singles]

-- ============================================================================
-- TÉCNICAS DE PROPAGACIÓN
-- ============================================================================

-- | NAKED SINGLES: Celdas que solo tienen un candidato posible
-- Esta es la técnica más básica y efectiva de propagación
--
-- Ejemplo:
--   Fila: [1,2,3,4,5,6,7,8,_]
--   El único candidato para _ es 9 → Naked Single
nakedSingles :: Board -> [(Position, Value)]
nakedSingles board =
    [ (pos, head cs)
    | pos <- indices board
    , board ! pos == Empty
    , let cs = candidates board pos
    , length cs == 1
    ]

-- | HIDDEN SINGLES: Valores que solo pueden ir en una posición dentro de una unidad
-- Verifica todas las filas, columnas y bloques
--
-- Ejemplo:
--   Fila: [1,2,_,4,5,_,7,8,_]
--   Si 3 solo puede ir en una de las celdas vacías → Hidden Single
hiddenSingles :: Board -> [(Position, Value)]
hiddenSingles board = 
    concat [ hiddenInRows board
           , hiddenInCols board  
           , hiddenInBlocks board
           ]

-- | Busca hidden singles en todas las filas
hiddenInRows :: Board -> [(Position, Value)]
hiddenInRows board = concat [hiddenInRow board r | r <- [0..8]]

-- | Busca hidden singles en una fila específica
hiddenInRow :: Board -> Int -> [(Position, Value)]
hiddenInRow board row =
    [ ((row, c), val)
    | val <- [1..9]
    , let emptyCols = [c | c <- [0..8], 
                           board ! (row, c) == Empty,
                           val `elem` candidates board (row, c)]
    , length emptyCols == 1
    , let [c] = emptyCols
    ]

-- | Busca hidden singles en todas las columnas
hiddenInCols :: Board -> [(Position, Value)]
hiddenInCols board = concat [hiddenInCol board c | c <- [0..8]]

-- | Busca hidden singles en una columna específica
hiddenInCol :: Board -> Int -> [(Position, Value)]
hiddenInCol board col =
    [ ((r, col), val)
    | val <- [1..9]
    , let emptyRows = [r | r <- [0..8],
                           board ! (r, col) == Empty,
                           val `elem` candidates board (r, col)]
    , length emptyRows == 1
    , let [r] = emptyRows
    ]

-- | Busca hidden singles en todos los bloques 3x3
hiddenInBlocks :: Board -> [(Position, Value)]
hiddenInBlocks board = concat [hiddenInBlock board b | b <- [0..8]]

-- | Busca hidden singles en un bloque 3x3 específico
-- Los bloques se numeran 0-8 de izquierda a derecha, arriba a abajo
hiddenInBlock :: Board -> Int -> [(Position, Value)]
hiddenInBlock board blockNum =
    [ (pos, val)
    | val <- [1..9]
    , let emptyPositions = [(r, c) | r <- [blockRow..blockRow+2],
                                      c <- [blockCol..blockCol+2],
                                      board ! (r, c) == Empty,
                                      val `elem` candidates board (r, c)]
    , length emptyPositions == 1
    , let [pos] = emptyPositions
    ]
  where
    blockRow = (blockNum `div` 3) * 3
    blockCol = (blockNum `mod` 3) * 3

-- ============================================================================
-- CÁLCULO DE CANDIDATOS Y VALIDACIÓN
-- ============================================================================

-- | Obtiene los valores candidatos válidos para una posición
-- Verifica que el valor no esté en la fila, columna ni bloque correspondientes
candidates :: Board -> Position -> [Value]
candidates board pos =
    [v | v <- [1..9], isValidPlacement board pos v]

-- | Verifica si colocar un valor en una posición es válido
-- Un valor es válido si no aparece en la fila, columna o bloque correspondiente
isValidPlacement :: Board -> Position -> Value -> Bool
isValidPlacement board (r, c) value =
    notInRow && notInCol && notInBlock
  where
    -- Verifica que el valor no esté en la fila
    notInRow =
        Filled value `notElem`
            [board ! (r, c') | c' <- [0..8]]

    -- Verifica que el valor no esté en la columna
    notInCol =
        Filled value `notElem`
            [board ! (r', c) | r' <- [0..8]]

    -- Verifica que el valor no esté en el bloque 3x3
    notInBlock =
        Filled value `notElem`
            [board ! (br, bc)
            | br <- [blockRow .. blockRow + 2]
            , bc <- [blockCol .. blockCol + 2]
            ]

    blockRow = (r `div` 3) * 3
    blockCol = (c `div` 3) * 3

-- ============================================================================
-- ESTADÍSTICAS Y DEBUGGING
-- ============================================================================

-- | Estadísticas de propagación para análisis
data PropagationStats = PropagationStats
    { nakedCount  :: Int
    , hiddenCount :: Int
    , iterations  :: Int
    } deriving (Show, Eq)

-- | Calcula estadísticas de propagación para un tablero
-- Útil para análisis y comparación de efectividad
propagationStats :: Board -> PropagationStats
propagationStats board = go board 0 0 0
  where
    go b nNaked nHidden nIter
        | null singles = PropagationStats nNaked nHidden nIter
        | otherwise    = 
            let naked = length (nakedSingles b)
                hidden = length (hiddenSingles b)
                newBoard = fillCells b singles
            in go newBoard (nNaked + naked) (nHidden + hidden) (nIter + 1)
      where
        singles = nakedSingles b ++ hiddenSingles b