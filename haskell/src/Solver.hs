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
  5. Si no, hacer backtracking (probar siguiente valor)
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
    | MostConstrained   -- ^ Llenar celda con menos opciones (MRV heuristic)
    deriving (Show, Eq)

-- | Resuelve un Sudoku usando backtracking con estrategia por defecto
solve :: Board -> Maybe Board
solve board
      | not (isValid board) = Nothing  -- Already resolved
      | otherwise = solveWithStrategy FirstEmpty board

-- | Resuelve un Sudoku usando una estrategia específica
solveWithStrategy :: SolveStrategy -> Board -> Maybe Board
solveWithStrategy strategy board
    | isSolved board = Just board
    | otherwise = case selectCell strategy board of
        Nothing -> Nothing  -- No hay celdas vacías pero no está resuelto (error)
        Just pos -> tryValues pos (candidates board pos)
  where
    -- Intenta colocar cada valor candidato en la posición
    tryValues :: Position -> [Value] -> Maybe Board
    tryValues _ [] = Nothing
    tryValues pos (v:vs) = 
        let newBoard = board // [(pos, Filled v)]
        in case solveWithStrategy strategy newBoard of
            Just solution -> Just solution
            Nothing -> tryValues pos vs

-- | Selecciona la siguiente celda a llenar según la estrategia
selectCell :: SolveStrategy -> Board -> Maybe Position
selectCell FirstEmpty board = findFirstEmpty board
selectCell MostConstrained board = findMostConstrained board

-- | Encuentra la primera celda vacía (búsqueda lineal)
findFirstEmpty :: Board -> Maybe Position
findFirstEmpty board = 
    case [pos | pos <- indices board, board ! pos == Empty] of
        [] -> Nothing
        (pos:_) -> Just pos

-- | Encuentra la celda vacía con menos candidatos (MRV - Minimum Remaining Values)
-- Esta heurística mejora dramáticamente el rendimiento
findMostConstrained :: Board -> Maybe Position
findMostConstrained board = 
    case emptyCells of
        [] -> Nothing
        cells -> Just $ minimumBy (comparing numCandidates) cells
  where
    emptyCells = [pos | pos <- indices board, board ! pos == Empty]
    numCandidates pos = length (candidates board pos)

-- | Obtiene los valores candidatos válidos para una posición
candidates :: Board -> Position -> [Value]
candidates board pos@(r, c) = 
    [v | v <- [1..9], isValidPlacement board pos v]

-- | Verifica si colocar un valor en una posición es válido
isValidPlacement :: Board -> Position -> Value -> Bool
isValidPlacement board (r, c) value =
    notInRow && notInCol && notInBlock
  where
    -- Verifica que el valor no esté en la fila
    notInRow = Filled value `notElem` [board ! (r, c') | c' <- [0..8]]
    
    -- Verifica que el valor no esté en la columna
    notInCol = Filled value `notElem` [board ! (r', c) | r' <- [0..8]]
    
    -- Verifica que el valor no esté en el bloque 3x3
    notInBlock = Filled value `notElem` blockCells
      where
        blockRow = (r `div` 3) * 3
        blockCol = (c `div` 3) * 3
        blockCells = [board ! (br, bc) 
                     | br <- [blockRow..blockRow+2]
                     , bc <- [blockCol..blockCol+2]]

-- ============================================================================
-- OPTIMIZACIONES ADICIONALES (Comentadas para comparación)
-- ============================================================================

{-
-- | Naked Singles: encuentra celdas con un solo candidato
nakedSingles :: Board -> [(Position, Value)]
nakedSingles board = 
    [(pos, head cs) | pos <- emptyCells, 
                      let cs = candidates board pos,
                      length cs == 1]
  where
    emptyCells = [pos | pos <- indices board, board ! pos == Empty]

-- | Constraint Propagation: propaga restricciones hasta punto fijo
propagate :: Board -> Board
propagate board = 
    case nakedSingles board of
        [] -> board  -- Punto fijo alcanzado
        singles -> propagate (board // [(pos, Filled val) | (pos, val) <- singles])

-- | Solver con propagación de restricciones
solveWithPropagation :: Board -> Maybe Board
solveWithPropagation board = solve' (propagate board)
  where
    solve' b
        | isSolved b = Just b
        | otherwise = case findMostConstrained b of
            Nothing -> Nothing
            Just pos -> tryValues pos (candidates b pos)
              where
                tryValues _ [] = Nothing
                tryValues p (v:vs) = 
                    let newBoard = propagate (b // [(p, Filled v)])
                    in case solve' newBoard of
                        Just solution -> Just solution
                        Nothing -> tryValues p vs
-}