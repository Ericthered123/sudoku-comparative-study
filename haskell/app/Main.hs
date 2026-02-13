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
import Control.Monad (when)

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
        ["--bench-external", strat, level] -> runExternalBench strat level
        ["--help"] -> printHelp
        _ -> printHelp >> exitFailure

-- | Modo interactivo con ejemplos predefinidos
runInteractive :: IO ()
runInteractive = do
    putStrLn "╔════════════════════════════════════════╗"
    putStrLn "║     SUDOKU SOLVER - HASKELL            ║"
    putStrLn "╚════════════════════════════════════════╝"
    putStrLn ""

    board <- selectDifficulty
    strategy <- selectStrategy

    solveAndPrint "RESOLVIENDO SUDOKU" board strategy

selectDifficulty :: IO Board
selectDifficulty = do
    putStrLn "Seleccione dificultad:"
    putStrLn "  1. Fácil"
    putStrLn "  2. Medio"
    putStrLn "  3. Difícil"
    putStrLn ""
    putStr "Opción (1-3): "

    option <- getLine
    case option of
        "1" -> return exampleEasy
        "2" -> return exampleMedium
        "3" -> return exampleHard
        _   -> putStrLn "Opción inválida\n" >> selectDifficulty

selectStrategy :: IO SolveStrategy
selectStrategy = do
    putStrLn ""
    putStrLn "Seleccione heurística:"
    putStrLn "  1. FirstEmpty"
    putStrLn "  2. MostConstrained"
    putStrLn "  3. PropagationMRV"
    putStrLn ""
    putStr "Opción (1-3): "

    option <- getLine
    case option of
        "1" -> return FirstEmpty
        "2" -> return MostConstrained
        "3" -> return PropagationMRV
        _   -> putStrLn "Opción inválida\n" >> selectStrategy

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
        
        -- FirstEmpty
        (sol1, time1) <- timeIt $ return $! solveWithStrategy FirstEmpty board
        case sol1 of
            Nothing -> putStrLn "  FirstEmpty:       No solution"
            Just _  ->
                printf "  FirstEmpty:       %.3f ms\n"
                    (realToFrac time1 * 1000 :: Double)
        
        -- MostConstrained
        (sol2, time2) <- timeIt $ return $! solveWithStrategy MostConstrained board
        case sol2 of
            Nothing -> putStrLn "  MostConstrained:  No solution"
            Just _  -> do
                printf "  MostConstrained:  %.3f ms\n"
                    (realToFrac time2 * 1000 :: Double)
                when (time1 > 0) $
                    printf "  Speedup vs FE:    %.2fx\n"
                        (realToFrac time1 / realToFrac time2 :: Double)

        -- PropagationMRV (≈ CLPFD)
        (sol3, time3) <- timeIt $ return $! solveWithStrategy PropagationMRV board
        case sol3 of
            Nothing -> putStrLn "  PropagationMRV:   No solution"
            Just _  -> do
                printf "  PropagationMRV:   %.3f ms\n"
                    (realToFrac time3 * 1000 :: Double)
                when (time3 > 0 && time2 > 0) $
                    printf "  Speedup vs MC:    %.2fx\n"
                        (realToFrac time2 / realToFrac time3 :: Double)

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

-- | Modo silencioso para benchmarking externo (usado por Python)
--   Imprime SOLO el tiempo en segundos.
runExternalBench :: String -> String -> IO ()
runExternalBench strat level = do
    let board = case level of
            "easy"   -> exampleEasy
            "medium" -> exampleMedium
            "hard"   -> exampleHard
            _        -> error "Invalid level"

        strategy = case strat of
            "fe"   -> FirstEmpty
            "mrv"  -> MostConstrained
            "pmrv" -> PropagationMRV
            _      -> error "Invalid strategy"

    -- Medición interna (solo solver)
    (_, time) <- timeIt $ return $! solveWithStrategy strategy board

    -- Imprimir SOLO número (segundos)
    print (realToFrac time :: Double)
