{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Sudoku
import Data.Array

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Types" $ do
        it "emptyBoard tiene 81 celdas vacías" $ do
            let board = emptyBoard
            length (elems board) `shouldBe` 81
            all (== Empty) (elems board) `shouldBe` True
        
        it "emptyBoard no está resuelto" $ do
            isSolved emptyBoard `shouldBe` False
    
    describe "Parsing" $ do
        it "parseBoard acepta formato de puntos" $ do
            let str = "530070000600195000098000060800060003400803001700020006060000280000419005000080079"
            parseBoard str `shouldSatisfy` (/= Nothing)
        
        it "parseBoard acepta formato de ceros" $ do
            let str = "530070000600195000098000060800060003400803001700020006060000280000419005000080079"
            parseBoard str `shouldSatisfy` (/= Nothing)
        
        it "parseBoard rechaza input inválido" $ do
            parseBoard "123" `shouldBe` Nothing
        
        it "parseBoardList acepta exactamente 81 elementos" $ do
            let list = replicate 81 0
            parseBoardList list `shouldSatisfy` (/= Nothing)
        
        it "parseBoardList rechaza listas incorrectas" $ do
            parseBoardList [1,2,3] `shouldBe` Nothing
    
    describe "Validation" $ do
        it "tablero vacío es válido" $ do
            isValid emptyBoard `shouldBe` True
        
        it "detecta duplicados en filas" $ do
            let Just board = parseBoard "110000000000000000000000000000000000000000000000000000000000000000000000000000000"
            isValid board `shouldBe` False
        
        it "detecta duplicados en columnas" $ do
            let Just board = parseBoard "100000000100000000000000000000000000000000000000000000000000000000000000000000000"
            isValid board `shouldBe` False
        
        it "detecta duplicados en bloques" $ do
            let Just board = parseBoard "110000000000000000000000000000000000000000000000000000000000000000000000000000000"
            isValid board `shouldBe` False
    
    describe "Candidates" $ do
        it "celda vacía en tablero vacío tiene 9 candidatos" $ do
            let cands = candidates emptyBoard (0,0)
            length cands `shouldBe` 9
        
        it "candidatos respetan restricciones de fila" $ do
            let Just board = parseBoard "123000000000000000000000000000000000000000000000000000000000000000000000000000000"
            let cands = candidates board (0,3)
            all (`notElem` [1,2,3]) cands `shouldBe` True
        
        it "candidatos respetan restricciones de columna" $ do
            let Just board = parseBoard "100000000200000000300000000000000000000000000000000000000000000000000000000000000"
            let cands = candidates board (3,0)
            all (`notElem` [1,2,3]) cands `shouldBe` True
        
        it "candidatos respetan restricciones de bloque" $ do
            let Just board = parseBoard "123000000456000000000000000000000000000000000000000000000000000000000000000000000"
            let cands = candidates board (2,2)
            all (`notElem` [1,2,3,4,5,6]) cands `shouldBe` True
    
    describe "Solver" $ do
        it "resuelve ejemplo fácil" $ do
            let solution = solve exampleEasy
            solution `shouldSatisfy` (/= Nothing)
            case solution of
                Just s -> isSolved s `shouldBe` True
                Nothing -> expectationFailure "No solution found"
        
        it "resuelve ejemplo medio" $ do
            let solution = solve exampleMedium
            solution `shouldSatisfy` (/= Nothing)
            case solution of
                Just s -> isSolved s `shouldBe` True
                Nothing -> expectationFailure "No solution found"
        
        it "resuelve ejemplo difícil con MostConstrained" $ do
            let solution = solveWithStrategy MostConstrained exampleHard
            solution `shouldSatisfy` (/= Nothing)
            case solution of
                Just s -> isSolved s `shouldBe` True
                Nothing -> expectationFailure "No solution found"
        
        it "tablero ya resuelto retorna el mismo tablero" $ do
            let Just board = parseBoard "534678912672195348198342567859761423426853791713924856961537284287419635345286179"
            solve board `shouldBe` Just board
        
        it "sudoku inválido retorna Nothing" $ do
            let Just board = parseBoard "110000000000000000000000000000000000000000000000000000000000000000000000000000000"
            solve board `shouldBe` Nothing
    
    describe "Strategies" $ do
        it "FirstEmpty y MostConstrained encuentran la misma solución" $ do
            let sol1 = solveWithStrategy FirstEmpty exampleEasy
            let sol2 = solveWithStrategy MostConstrained exampleEasy
            sol1 `shouldBe` sol2
    
    describe "Properties (QuickCheck)" $ do
        it "cualquier sudoku resuelto es válido" $ property $
            \() -> 
                case solve exampleEasy of
                    Just s -> isValid s
                    Nothing -> False
        
        it "solución preserva las pistas iniciales" $ property $
            \() ->
                case solve exampleEasy of
                    Just solution ->
                        all (\pos -> 
                            case exampleEasy ! pos of
                                Empty -> True
                                Filled n -> solution ! pos == Filled n
                        ) (indices exampleEasy)
                    Nothing -> False