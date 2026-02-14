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
import System.Clock (getTime, Clock(Monotonic), toNanoSecs)
import System.IO
import Types

-- ============================================================================
-- PARSING
-- ============================================================================

-- | Parsea un tablero desde una string de 81 caracteres.
-- Acepta dígitos 1-9 como valores y '.' o '0' como celdas vacías.
-- Ignora cualquier otro carácter (saltos de línea, espacios, separadores).
-- Retorna Nothing si el resultado no tiene exactamente 81 celdas válidas.
--
-- Ejemplos válidos:
--   "530070000600195000..."          (formato compacto, 81 chars)
--   "5 3 0 | 0 7 0 | 0 0 0\n..."    (formato con separadores, se ignoran)
parseBoard :: String -> Maybe Board
parseBoard str
    | length digits == 81 = Just $ listArray ((0,0), (8,8)) (map toCell digits)
    | otherwise           = Nothing
  where
    digits = filter isValidChar str
    isValidChar c = isDigit c || c == '.'

    toCell '.' = Empty
    toCell '0' = Empty
    toCell c   = Filled (digitToInt c)

-- | Parsea un tablero desde una lista de 81 enteros.
-- 0 = celda vacía; 1-9 = valor fijo.
parseBoardList :: [Int] -> Maybe Board
parseBoardList xs
    | length xs == 81 = Just $ listArray ((0,0), (8,8)) (map toCell xs)
    | otherwise       = Nothing
  where
    toCell 0 = Empty
    toCell n | n >= 1 && n <= 9 = Filled n
             | otherwise        = Empty

-- | Convierte un tablero a string compacto de 81 dígitos (0 = vacío).
boardToString :: Board -> String
boardToString board = concatMap cellToChar (elems board)
  where
    cellToChar Empty      = "0"
    cellToChar (Filled n) = show n

-- ============================================================================
-- I/O
-- ============================================================================

-- | Lee un tablero desde un archivo de texto.
readBoardFromFile :: FilePath -> IO (Maybe Board)
readBoardFromFile path = do
    content <- readFile path
    return $ parseBoard content

-- | Escribe un tablero formateado a un archivo.
writeBoardToFile :: FilePath -> Board -> IO ()
writeBoardToFile path board = writeFile path (prettyBoard board)

-- ============================================================================
-- MEDICIÓN DE TIEMPO
--
-- timeIt usa getTime Monotonic (paquete `clock`) en lugar de getCPUTime.
--
-- Por qué no getCPUTime:
--   getCPUTime en Windows usa GetProcessTimes (Win32), que tiene resolución
--   de 15.625 ms (timer a 64 Hz) — igual que statistics(cputime) en Prolog.
--   Produce 0.000000000 o 0.015625000 alternadamente para puzzles rápidos.
--
-- Por qué getTime Monotonic:
--   Usa QueryPerformanceCounter en Windows → resolución ~100 ns real.
--   Es el mismo mecanismo que SWI-Prolog usa internamente para get_time/1,
--   lo que hace ambas métricas comparables entre sí.
--   Para un solver single-threaded sin I/O, monotonic ≈ CPU time.
--
-- toNanoSecs convierte TimeSpec → Integer nanosegundos.
-- Dividir por 1e9 → segundos (Double).
-- ============================================================================

-- | Mide el tiempo de ejecución de una acción IO.
-- Retorna el resultado y el tiempo transcurrido en segundos (Double).
timeIt :: IO a -> IO (a, Double)
timeIt action = do
    t1     <- getTime Monotonic
    result <- action
    t2     <- getTime Monotonic
    let elapsedSecs = fromIntegral (toNanoSecs t2 - toNanoSecs t1) / (1e9 :: Double)
    return (result, elapsedSecs)

-- ============================================================================
-- EJEMPLOS PREDEFINIDOS
-- ============================================================================

-- | Sudoku fácil (~30 pistas)
exampleEasy :: Board
exampleEasy = case parseBoard str of
    Just b  -> b
    Nothing -> error "exampleEasy: tablero inválido"
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

-- | Sudoku medio (~25 pistas)
exampleMedium :: Board
exampleMedium = case parseBoard str of
    Just b  -> b
    Nothing -> error "exampleMedium: tablero inválido"
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
    Just b  -> b
    Nothing -> error "exampleHard: tablero inválido"
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