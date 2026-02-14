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
import System.Clock (getTime, Clock(Monotonic), toNanoSecs)

main :: IO ()
main = do
    setLocaleEncoding utf8
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hSetEncoding stdin utf8
    args <- getArgs
    processArgs args

-- | Procesa los argumentos de forma segura
processArgs :: [String] -> IO ()
processArgs ("--bench-external":strat:rest) =
    runExternalBench strat (concat rest)
processArgs ("--example":level:_) =
    runExample level
processArgs ("--file":path:_) =
    runFromFile path
processArgs ["--benchmark"] =
    runBenchmark
processArgs ["--help"] =
    printHelp
processArgs [] =
    runInteractive
processArgs _ =
    putStrLn "Argumentos inválidos" >> printHelp >> exitFailure

-- | Modo interactivo con ejemplos predefinidos
runInteractive :: IO ()
runInteractive = do
    putStrLn "╔════════════════════════════════════════╗"
    putStrLn "║     SUDOKU SOLVER - HASKELL            ║"
    putStrLn "╚════════════════════════════════════════╝"
    putStrLn ""
    board    <- selectDifficulty
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
    "easy"   -> solveAndPrint "FACIL"   exampleEasy   FirstEmpty
    "medium" -> solveAndPrint "MEDIO"   exampleMedium FirstEmpty
    "hard"   -> solveAndPrint "DIFICIL" exampleHard   MostConstrained
    _        -> putStrLn "Nivel invalido. Use: easy, medium, hard" >> exitFailure

-- | Lee y resuelve un sudoku desde archivo
runFromFile :: FilePath -> IO ()
runFromFile path = do
    maybeBoard <- readBoardFromFile path
    case maybeBoard of
        Nothing    -> putStrLn "Error: Formato de archivo invalido" >> exitFailure
        Just board -> solveAndPrint ("ARCHIVO: " ++ path) board FirstEmpty

-- | Ejecuta benchmarks comparando estrategias (modo interactivo)
runBenchmark :: IO ()
runBenchmark = do
    putStrLn "╔════════════════════════════════════════╗"
    putStrLn "║          BENCHMARK MODE                ║"
    putStrLn "╚════════════════════════════════════════╝"
    putStrLn ""
    let boards = [ ("Facil",   exampleEasy)
                 , ("Medio",   exampleMedium)
                 , ("Dificil", exampleHard)
                 ]
    mapM_ benchmarkBoard boards
  where
    benchmarkBoard (name, board) = do
        putStrLn $ "─── " ++ name ++ " " ++ replicate (35 - length name) '─'

        -- timeIt ahora devuelve (resultado, segundos :: Double)
        (sol1, t1) <- timeIt $ return $! solveWithStrategy FirstEmpty board
        case sol1 of
            Nothing -> putStrLn "  FirstEmpty:       No solution"
            Just _  -> printf "  FirstEmpty:       %.3f ms\n" (t1 * 1000)

        (sol2, t2) <- timeIt $ return $! solveWithStrategy MostConstrained board
        case sol2 of
            Nothing -> putStrLn "  MostConstrained:  No solution"
            Just _  -> do
                printf "  MostConstrained:  %.3f ms\n" (t2 * 1000)
                when (t1 > 0) $
                    printf "  Speedup vs FE:    %.2fx\n" (t1 / t2)

        (sol3, t3) <- timeIt $ return $! solveWithStrategy PropagationMRV board
        case sol3 of
            Nothing -> putStrLn "  PropagationMRV:   No solution"
            Just _  -> do
                printf "  PropagationMRV:   %.3f ms\n" (t3 * 1000)
                when (t3 > 0 && t2 > 0) $
                    printf "  Speedup vs MC:    %.2fx\n" (t2 / t3)
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
    -- timeIt devuelve segundos; multiplicar por 1000 para ms en printf
    (result, t) <- timeIt $ return $! solveWithStrategy strategy board
    case result of
        Nothing       -> putStrLn "✗ No se encontro solucion"
        Just solution -> do
            printf "✓ Resuelto en %.3f ms (estrategia: %s)\n\n"
                   (t * 1000) (show strategy)
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
    , "  (sin argumentos)                   Modo interactivo"
    , "  --example NIVEL                    Resolver ejemplo (easy, medium, hard)"
    , "  --file ARCHIVO                     Resolver desde archivo"
    , "  --benchmark                        Benchmarks con ejemplos predefinidos"
    , "  --bench-external STRAT PUZZLE81    Benchmark externo (usado por Python)"
    , "  --help                             Mostrar esta ayuda"
    , ""
    , "ESTRATEGIAS para --bench-external:"
    , "  fe   → FirstEmpty"
    , "  mrv  → MostConstrained (MRV)"
    , "  pmrv → PropagationMRV"
    , ""
    , "PUZZLE81: cadena de exactamente 81 caracteres"
    , "  Dígitos 1-9 = valor fijo; '0' o '.' = celda vacía"
    , "  Ejemplo:"
    , "  sudoku-exe --bench-external mrv 530070000600195000098000060800060003400803001700020006060000280000419005000080079"
    ]

-- ─────────────────────────────────────────────────────────────────────────────
-- | Modo silencioso para benchmarking externo (invocado por Python).
--
--   Firma de llamada:
--     sudoku-exe --bench-external <strat> <puzzle81>
--
--   Donde puzzle81 es una cadena de 81 chars (0/. = vacío, 1-9 = fijo).
--   Python lo pasa directamente desde su lista de puzzles del archivo.
--
--   Salida: SOLO el tiempo en segundos como float (ej: "0.003421000")
--   Python lo lee con float(stdout) y multiplica por 1000 para obtener ms.
--
--   Medición con getCPUTime:
--     t1, t2 en picosegundos → (t2-t1)/1e12 = segundos con resolución sub-ms.
--
--   Lazy evaluation: `result \`seq\` return ()` fuerza la evaluación completa
--   del solver ANTES de tomar t2. Sin esto, Haskell diferiría el cómputo
--   hasta después de la medición y daría siempre ~0 ns.
-- ─────────────────────────────────────────────────────────────────────────────
runExternalBench :: String -> String -> IO ()
runExternalBench strat puzzleStr = do
    let strategy = case strat of
            "fe"   -> FirstEmpty
            "mrv"  -> MostConstrained
            "pmrv" -> PropagationMRV
            _      -> error $ "Estrategia inválida: " ++ strat

    case parseBoard puzzleStr of
        Nothing -> do
            hPutStrLn stderr $
                "Error: puzzle inválido — necesita 81 celdas, "
                ++ "recibido: " ++ show (length (filter (\c -> c `elem` ".0123456789") puzzleStr))
                ++ " celdas válidas"
            exitFailure
        Just board -> do
            t1 <- getTime Monotonic
            let result = solveWithStrategy strategy board
            result `seq` return ()   -- forzar evaluación antes de t2
            t2 <- getTime Monotonic
            let elapsedSecs = fromIntegral (toNanoSecs t2 - toNanoSecs t1) / (1e9 :: Double)
            printf "%.9f\n" elapsedSecs