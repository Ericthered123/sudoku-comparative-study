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
import GHC.IO.Encoding (setLocaleEncoding)


main :: IO ()
main = do
    setLocaleEncoding utf8
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
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
    putStrLn "Please select an example:"
    putStrLn "  1. Easy (FirstEmpty)"
    putStrLn "  2. Medio (FirstEmpty)"
    putStrLn "  3. Medio (MostConstrained)"
    putStrLn "  4. Dificil (MostConstrained)"
    putStrLn ""
    putStr "Opcion (1-4): "
    
    option <- getLine
    case option of
        "1" -> solveAndPrint "FACIL (FirstEmpty)" exampleEasy FirstEmpty
        "2" -> solveAndPrint "MEDIO (FirstEmpty)" exampleMedium FirstEmpty
        "3" -> solveAndPrint "MEDIO (MostConstrained)" exampleMedium MostConstrained
        "4" -> solveAndPrint "DIFICIL (MostConstrained)" exampleHard MostConstrained
        _ -> putStrLn "Opcion invalida" >> exitFailure

-- | Ejecuta un ejemplo específico
runExample :: String -> IO ()
runExample level = case level of
    "easy" -> solveAndPrint "FACIL" exampleEasy FirstEmpty
    "medium" -> solveAndPrint "MEDIO" exampleMedium FirstEmpty
    "hard" -> solveAndPrint "DIFICIL" exampleHard MostConstrained
    _ -> putStrLn "Nivel invalido. Use: easy, medium, hard" >> exitFailure

-- | Lee y resuelve un sudoku desde archivo
runFromFile :: FilePath -> IO ()
runFromFile path = do
    maybeBoard <- readBoardFromFile path
    case maybeBoard of
        Nothing -> putStrLn "Error: Formato de archivo invalido" >> exitFailure
        Just board -> solveAndPrint ("ARCHIVO: " ++ path) board FirstEmpty

-- | Ejecuta benchmarks comparando estrategias
runBenchmark :: IO ()
runBenchmark = do
    putStrLn "╔════════════════════════════════════════╗"
    putStrLn "║          BENCHMARK MODE                ║"
    putStrLn "╚════════════════════════════════════════╝"
    putStrLn ""
    
    let boards = [ ("Facil", exampleEasy)
                 , ("Medio", exampleMedium)
                 , ("Dificil", exampleHard)
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
            putStrLn "✗ No se encontro solucion"
        Just solution -> do
            printf "✓ Resuelto en %.3f ms (estrategia: %s)\n\n"
                   (realToFrac time * 1000 :: Double)
                   (show strategy)
            putStrLn "╔════════════════════════════════════════╗"
            putStrLn ("║ SOLUCION:" ++ replicate 29 ' ' ++ "║")
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