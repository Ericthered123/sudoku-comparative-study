{-|
Module      : Solver
Description : Algoritmos para resolver Sudoku usando backtracking
Copyright   : (c) 2026 Eric Doyle
License     : BSD3
Maintainer  : eric6doyle@gmail.com

Este módulo implementa el algoritmo de resolución de Sudoku usando
backtracking funcional puro. No usa mutabilidad ni efectos secundarios.
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
  - Propagación de restricciones + MRV (estilo CLP(FD))
-}  

module Solver
    ( solve
    , solveWithStrategy
    , SolveStrategy(..)
    , candidates
    , isValidPlacement
    ) where

import Data.Array
import Data.List (minimumBy, sortBy)
import Data.Ord (comparing)
import Types
import Data.Maybe (Maybe(Nothing))

-- | Estrategias de resolución
data SolveStrategy 
    = FirstEmpty        -- ^ Llenar primera celda vacía (más simple)
    | MostConstrained   -- ^ Fill cell with fewer options (MRV heuristic)
    | PropagationMRV    -- ^ Propagación + MRV (comparable a CLP(FD))
    deriving (Show, Eq)

-- | Solver by default
solve :: Board -> Maybe Board
solve board
    | not (isValid board) = Nothing
    | otherwise           = solveWithStrategy FirstEmpty board

-- | Solver parametrizado por estrategia
solveWithStrategy :: SolveStrategy -> Board -> Maybe Board
solveWithStrategy strategy board =
    case preprocess strategy board of
        Nothing -> Nothing
        Just b
            | isSolved b -> Just b
            | otherwise ->
                case selectCell strategy b of
                    Nothing  -> Nothing
                    Just pos -> tryValues b pos (candidates b pos)
  where
    tryValues :: Board -> Position -> [Value] -> Maybe Board
    tryValues _ _ [] = Nothing
    tryValues b pos (v:vs) =
        case solveWithStrategy strategy (b // [(pos, Filled v)]) of
            Just sol -> Just sol
            Nothing  -> tryValues b pos vs

-- ============================================================================
-- Selección de variable
-- ============================================================================

selectCell :: SolveStrategy -> Board -> Maybe Position
selectCell FirstEmpty      board = findFirstEmpty board
selectCell MostConstrained board = findMostConstrained board
selectCell PropagationMRV  board = findMostConstrained board

findFirstEmpty :: Board -> Maybe Position
findFirstEmpty board =
    case [pos | pos <- indices board, board ! pos == Empty] of
        []      -> Nothing
        (p : _) -> Just p

findMostConstrained :: Board -> Maybe Position
findMostConstrained board =
    case emptyCells of
        []    -> Nothing
        cells -> Just $ minimumBy (comparing numCandidates) cells
  where
    emptyCells = [pos | pos <- indices board, board ! pos == Empty]
    numCandidates pos = length (candidates board pos)

-- ============================================================================
-- Propagación de restricciones (estilo CLP(FD))
-- ============================================================================

preprocess :: SolveStrategy -> Board -> Maybe Board
preprocess PropagationMRV board = propagate board
preprocess _              board = Just board

propagate :: Board -> Maybe Board
propagate board
    | hasEmptyDomain board = Nothing
    | null singles         = Just board
    | otherwise            =
        propagate (board // [(pos, Filled v) | (pos, v) <- singles])
  where
    singles = nakedSingles board

hasEmptyDomain :: Board -> Bool
hasEmptyDomain board =
    any (\pos -> null (candidates board pos))
        [pos | pos <- indices board, board ! pos == Empty]

nakedSingles :: Board -> [(Position, Value)]
nakedSingles board =
    [ (pos, v)
    | pos <- indices board
    , board ! pos == Empty
    , let cs = candidates board pos
    , [v] <- [cs]
    ]

-- ============================================================================
-- Restricciones
-- ============================================================================

candidates :: Board -> Position -> [Value]
candidates board pos =
    [v | v <- [1..9], isValidPlacement board pos v]

isValidPlacement :: Board -> Position -> Value -> Bool
isValidPlacement board (r, c) value =
    notInRow && notInCol && notInBlock
  where
    notInRow =
        Filled value `notElem`
            [board ! (r, c') | c' <- [0..8]]

    notInCol =
        Filled value `notElem`
            [board ! (r', c) | r' <- [0..8]]

    notInBlock =
        Filled value `notElem`
            [board ! (br, bc)
            | br <- [blockRow .. blockRow + 2]
            , bc <- [blockCol .. blockCol + 2]
            ]

    blockRow = (r `div` 3) * 3
    blockCol = (c `div` 3) * 3