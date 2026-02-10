{-|
Module      : Sudoku
Description : Módulo principal del solucionador de Sudoku
Copyright   : (c) 2026 Eric Doyle
License     : BSD3
Maintainer  : eric6doyle@gmail.com


Este módulo re-exporta todas las funcionalidades del solucionador
de Sudoku para facilitar su uso.

Ejemplo de uso:

@
import Sudoku

main :: IO ()
main = do
    let board = exampleEasy
    putStrLn "Puzzle:"
    putStrLn $ prettyBoard board
    
    case solve board of
        Nothing -> putStrLn "No solution found"
        Just solution -> do
            putStrLn "\\nSolution:"
            putStrLn $ prettyBoard solution
@
-}

module Sudoku
    ( -- * Tipos de datos
      Cell(..)
    , Board
    , Position
    , Value
    
    -- * Creación de tableros
    , emptyBoard
    , parseBoard
    , parseBoardList
    , boardToString
    
    -- * Resolución
    , solve
    , solveWithStrategy
    , SolveStrategy(..)
    
    -- * Validación
    , isValid
    , isSolved
    , isValidPlacement
    , candidates
    
    -- * I/O
    , prettyBoard
    , readBoardFromFile
    , writeBoardToFile
    
    -- * Utilidades
    , timeIt
    
    -- * Ejemplos
    , exampleEasy
    , exampleMedium
    , exampleHard
    ) where

import Types
import Solver
import Utils