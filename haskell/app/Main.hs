{-|
Module      : Main
Description : Aplicación CLI para resolver Sudokus
Copyright   : (c) 2026 Eric Doyle
-}

module Main (main) where

import Sudoku
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)
import System.IO


main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8
    args <- getArgs
    case args of
        [] -> runInteractive
        ["--example", level] -> runExample level
        ["--file", path] -> runFromFile path
        ["--benchmark"] -> runBenchmark
        ["--help"] -> printHelp
        _ -> printHelp >> exitFailure

-- | Modo interactivo con ejemplos predefinidos
runInteractive :: IO ()
runInteractive = do
    putStrLn "╔════════════════════════════════════════╗"
    putStrLn "║     SUDOKU SOLVER - HASKELL            ║"
    putStrLn "╚════════════════════════════════════════╝"
    putStrLn ""
    putStrLn "Seleccione un ejemplo:"
    putStrLn "  1. Fácil"
    putStrLn "  2. Medio"
    putStrLn "  3. Difícil"
    putStrLn ""
    putStr "Opción (1-3): "
    
    option <- getLine
    case option of
        "1" -> solveAndPrint "FÁCIL" exampleEasy FirstEmpty
        "2" -> solveAndPrint "MEDIO" exampleMedium FirstEmpty
        "3" -> solveAndPrint "DIFÍCIL" exampleHard MostConstrained
        _ -> putStrLn "Opción inválida" >> exitFailure

-- | Ejecuta un ejemplo específico
runExample :: String -> IO ()
runExample level = case level of
    "easy" -> solveAndPrint "FÁCIL" exampleEasy FirstEmpty
    "medium" -> solveAndPrint "MEDIO" exampleMedium FirstEmpty
    "hard" -> solveAndPrint "DIFÍCIL" exampleHard MostConstrained
    _ -> putStrLn "Nivel inválido. Use: easy, medium, hard" >> exitFailure

-- | Lee y resuelve un sudoku desde archivo
runFromFile :: FilePath -> IO ()
runFromFile path = do
    maybeBoard <- readBoardFromFile path
    case maybeBoard of
        Nothing -> putStrLn "Error: Formato de archivo inválido" >> exitFailure
        Just board -> solveAndPrint ("ARCHIVO: " ++ path) board FirstEmpty

-- | Ejecuta benchmarks comparando estrategias
runBenchmark :: IO ()
runBenchmark = do
    putStrLn "╔════════════════════════════════════════╗"
    putStrLn "║          BENCHMARK MODE                ║"
    putStrLn "╚════════════════════════════════════════╝"
    putStrLn ""
    
    let boards = [ ("Fácil", exampleEasy)
                 , ("Medio", exampleMedium)
                 , ("Difícil", exampleHard)
                 ]
    
    mapM_ benchmarkBoard boards
  where
    benchmarkBoard (name, board) = do
        putStrLn $ "─── " ++ name ++ " " ++ replicate (35 - length name) '─'
        
        -- Estrategia FirstEmpty
        (sol1, time1) <- timeIt $ return $! solveWithStrategy FirstEmpty board
        case sol1 of
            Nothing -> putStrLn "  FirstEmpty:       No solution"
            Just _ -> printf "  FirstEmpty:       %.3f ms\n" (realToFrac time1 * 1000 :: Double)
        
        -- Estrategia MostConstrained
        (sol2, time2) <- timeIt $ return $! solveWithStrategy MostConstrained board
        case sol2 of
            Nothing -> putStrLn "  MostConstrained:  No solution"
            Just _ -> do
                printf "  MostConstrained:  %.3f ms\n" (realToFrac time2 * 1000 :: Double)
                if time1 > 0 
                    then printf "  Speedup:          %.2fx\n" (realToFrac time1 / realToFrac time2 :: Double)
                    else return ()
        
        putStrLn ""

-- | Resuelve un tablero y muestra el resultado
solveAndPrint :: String -> Board -> SolveStrategy -> IO ()
solveAndPrint name board strategy = do
    putStrLn $ "╔════ " ++ name ++ " " ++ replicate (34 - length name) '═' ++ "╗"
    putStrLn ("║ PUZZLE:" ++ replicate 31 ' ' ++ "║")
    putStrLn "╚════════════════════════════════════════╝"
    putStrLn ""
    putStrLn $ prettyBoard board
    putStrLn ""
    
    putStrLn "Resolviendo..."
    (result, time) <- timeIt $ return $! solveWithStrategy strategy board
    
    case result of
        Nothing -> do
            putStrLn "✗ No se encontró solución"
        Just solution -> do
            printf "✓ Resuelto en %.3f ms (estrategia: %s)\n\n"
                   (realToFrac time * 1000 :: Double)
                   (show strategy)
            putStrLn "╔════════════════════════════════════════╗"
            putStrLn ("║ SOLUCIÓN:" ++ replicate 29 ' ' ++ "║")
            putStrLn "╚════════════════════════════════════════╝"
            putStrLn ""
            putStrLn $ prettyBoard solution

-- | Imprime ayuda
printHelp :: IO ()
printHelp = putStrLn $ unlines
    [ "Uso: sudoku-exe [OPCIONES]"
    , ""
    , "OPCIONES:"
    , "  (sin argumentos)       Modo interactivo"
    , "  --example NIVEL        Resolver ejemplo (easy, medium, hard)"
    , "  --file ARCHIVO         Resolver desde archivo"
    , "  --benchmark            Ejecutar benchmarks"
    , "  --help                 Mostrar esta ayuda"
    , ""
    , "EJEMPLOS:"
    , "  sudoku-exe"
    , "  sudoku-exe --example easy"
    , "  sudoku-exe --file puzzle.txt"
    , "  sudoku-exe --benchmark"
    ]