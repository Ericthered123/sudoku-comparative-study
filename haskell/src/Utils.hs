{-|
Module      : Utils
Description : Utilidades para parsing, I/O y benchmarking
Copyright   : (c) 2026 Eric Doyle
License     : BSD3
Maintainer  : eric6doyle@gmail.com


Funciones auxiliares para:
  - Parsear sudokus desde strings
  - Leer/escribir archivos
  - Medir tiempos de ejecución
-}

module Utils
    ( parseBoard
    , parseBoardList
    , boardToString
    , readBoardFromFile
    , writeBoardToFile
    , timeIt
    , exampleEasy
    , exampleMedium
    , exampleHard
    ) where

import Data.Array
import Data.Char (isDigit, digitToInt)
import Data.Time.Clock
import System.IO
import Types

-- | Parsea un tablero desde una string
-- Formato: números 1-9 y '.' o '0' para celdas vacías
-- Ejemplo: "53..7....6..195....98....6.8...6...3..."
parseBoard :: String -> Maybe Board
parseBoard str
    | length digits == 81 = Just $ listArray ((0,0), (8,8)) (map toCell digits)
    | otherwise = Nothing
  where
    -- Filtrar solo caracteres válidos
    digits = filter isValidChar str
    isValidChar c = isDigit c || c == '.'
    
    toCell '.' = Empty
    toCell '0' = Empty
    toCell c = Filled (digitToInt c)

-- | Parsea un tablero desde una lista de 81 elementos
-- Formato: [5,3,0,0,7,0,0,0,0, 6,0,0,1,9,5,0,0,0, ...]
parseBoardList :: [Int] -> Maybe Board
parseBoardList xs
    | length xs == 81 = Just $ listArray ((0,0), (8,8)) (map toCell xs)
    | otherwise = Nothing
  where
    toCell 0 = Empty
    toCell n | n >= 1 && n <= 9 = Filled n
             | otherwise = Empty

-- | Convierte un tablero a string (formato compacto)
boardToString :: Board -> String
boardToString board = concatMap cellToChar (elems board)
  where
    cellToChar Empty = "0"
    cellToChar (Filled n) = show n

-- | Lee un tablero desde un archivo
readBoardFromFile :: FilePath -> IO (Maybe Board)
readBoardFromFile path = do
    content <- readFile path
    return $ parseBoard content

-- | Escribe un tablero a un archivo
writeBoardToFile :: FilePath -> Board -> IO ()
writeBoardToFile path board = do
    writeFile path (prettyBoard board)

-- | Mide el tiempo de ejecución de una función
timeIt :: IO a -> IO (a, NominalDiffTime)
timeIt action = do
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    return (result, diffUTCTime end start)

-- ============================================================================
-- EJEMPLOS PREDEFINIDOS
-- ============================================================================

-- | Sudoku fácil (30 pistas)
exampleEasy :: Board
exampleEasy = case parseBoard str of
    Just b -> b
    Nothing -> error "Invalid example board"
  where
    str = "530070000\
          \600195000\
          \098000060\
          \800060003\
          \400803001\
          \700020006\
          \060000280\
          \000419005\
          \000080079"

-- | Sudoku medio (25 pistas)
exampleMedium :: Board
exampleMedium = case parseBoard str of
    Just b -> b
    Nothing -> error "Invalid example board"
  where
    str = "000000000\
          \000003085\
          \001020000\
          \000507000\
          \004000100\
          \090000000\
          \500000073\
          \002010000\
          \000040009"

-- | Sudoku difícil (World's Hardest - Arto Inkala)
exampleHard :: Board
exampleHard = case parseBoard str of
    Just b -> b
    Nothing -> error "Invalid example board"
  where
    str = "800000000\
          \003600000\
          \070090200\
          \050007000\
          \000045700\
          \000100030\
          \001000068\
          \008500010\
          \090000400"

-- ============================================================================
-- FUNCIONES DE TESTING
-- ============================================================================

-- | Genera una representación visual ASCII de múltiples tableros
showBoards :: [(String, Board)] -> String
showBoards boards = unlines $ concatMap formatBoard boards
  where
    formatBoard (name, board) = 
        [replicate 40 '=', name, replicate 40 '='] 
        ++ lines (prettyBoard board)
        ++ [""]

-- | Convierte un tablero a formato de lista (para benchmarks)
boardToList :: Board -> [Int]
boardToList board = map cellToInt (elems board)
  where
    cellToInt Empty = 0
    cellToInt (Filled n) = n