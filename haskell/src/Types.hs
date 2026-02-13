{-|
Module      : Types
Description : Tipos de datos para el solucionador de Sudoku
Copyright   : (c) 2026 Eric Doyle
License     : BSD3
Maintainer  : eric6doyle@gmail.com


Este módulo define los tipos de datos fundamentales para representar
y manipular tableros de Sudoku.
-}

module Types
    ( Cell(..)
    , Board
    , Position
    , Value
    , emptyBoard
    , isValid
    , isSolved
    , prettyBoard
    ) where

import Data.Array
import Data.List (intercalate)

-- | Tipo para representar una celda del Sudoku
-- Puede estar vacía (Empty) o tener un valor (Filled)
data Cell = Empty | Filled Int
    deriving (Eq)

instance Show Cell where
    show Empty = "."
    show (Filled n) = show n

-- | Tipo para los valores válidos del Sudoku (1-9)
type Value = Int

-- | Posición en el tablero (fila, columna)
-- Ambas van de 0 a 8
type Position = (Int, Int)

-- | Tablero representado como un array bidimensional
-- Usa Array para acceso O(1) eficiente
type Board = Array Position Cell

-- | Crea un tablero vacío 9x9
emptyBoard :: Board
emptyBoard = array ((0,0), (8,8)) [((i,j), Empty) | i <- [0..8], j <- [0..8]]

-- | Verifica si un tablero es válido (no hay duplicados en filas/cols/bloques)
isValid :: Board -> Bool
isValid board = all checkUnique (rows ++ cols ++ blocks)
  where
    rows = [[board ! (r,c) | c <- [0..8]] | r <- [0..8]]
    cols = [[board ! (r,c) | r <- [0..8]] | c <- [0..8]]
    blocks = [[board ! (r,c) | r <- [br*3..br*3+2], c <- [bc*3..bc*3+2]] 
             | br <- [0..2], bc <- [0..2]]
    
    -- Verifica que no haya valores duplicados (ignorando Empty)
    checkUnique :: [Cell] -> Bool
    checkUnique cells = 
        let values = [n | Filled n <- cells]
        in values == nub values
      where
        nub [] = []
        nub (x:xs) = x : nub (filter (/= x) xs)

-- | Verifica si el tablero está completamente resuelto
isSolved :: Board -> Bool
isSolved board = all isFilled (elems board) && isValid board
  where
    isFilled Empty = False
    isFilled (Filled _) = True

-- | Imprime el tablero de forma legible
prettyBoard :: Board -> String
prettyBoard board = unlines $ addSeparators $ map formatRow [0..8]
  where
    formatRow r = intercalate " | " [formatBlock r b | b <- [0,3,6]]
    
    formatBlock r c = unwords [showCell (board ! (r, c+i)) | i <- [0..2]]
    
    showCell Empty = "."
    showCell (Filled n) = show n
    
    separator = replicate 21 '-'
    
    addSeparators rows = 
        let (top, rest) = splitAt 3 rows
            (mid, bot) = splitAt 3 rest
        in top ++ [separator] ++ mid ++ [separator] ++ bot