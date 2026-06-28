module Main where

import Control.Monad (forM_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Exit (exitFailure)

import SpreadsheetLang

type Test = (String, Either String ())

main :: IO ()
main = do
    results <- mapM runTest tests
    let failures = filter (not . snd) results
    if null failures
        then putStrLn ("All " ++ show (length results) ++ " tests passed.")
        else exitFailure

runTest :: Test -> IO (String, Bool)
runTest (name, result) =
    case result of
        Right () -> do
            putStrLn ("[pass] " ++ name)
            pure (name, True)
        Left message -> do
            putStrLn ("[fail] " ++ name ++ ": " ++ message)
            pure (name, False)

tests :: [Test]
tests =
    [ testParserCellsAndComments
    , testParserPrecedence
    , testDependencyGraph
    , testEvaluationOrder
    , testEndToEndEvaluation
    , testDivisionByZero
    , testMissingReference
    , testTypeError
    , testCycleDetection
    , testCycleDoesNotHang
    , testRecomputeNoChangeInvariant
    , testOrderIndependenceInvariant
    , testAffectedCells
    , testReactiveMatchesFullEvaluation
    , testReactiveLeavesUnrelatedValues
    , testReactiveCanCreateCycle
    ]

testParserCellsAndComments :: Test
testParserCellsAndComments =
    ( "parser accepts comments, literals, formulas, and ranges"
    , do
        sheet <- parseOk sampleSheet
        let cells = sheetCells sheet
        assertEqual "cell count" 6 (Map.size cells)
        assertEqual "A1 literal" (Just (Lit (NumV 10))) (Map.lookup (addr "A" 1) cells)
        assertEqual
            "A4 range"
            (Just (Form (RangeOp SumR (addr "A" 1) (addr "A" 3))))
            (Map.lookup (addr "A" 4) cells)
    )

testParserPrecedence :: Test
testParserPrecedence =
    ( "parser keeps multiplication before addition"
    , do
        expr <- parseExprOk "A1 + 2 * B3"
        assertEqual
            "AST"
            (BinOp Add (Ref (addr "A" 1)) (BinOp Mul (LitE (NumV 2)) (Ref (addr "B" 3))))
            expr
    )

testDependencyGraph :: Test
testDependencyGraph =
    ( "dependency graph includes references from formulas and ranges"
    , do
        sheet <- parseOk sampleSheet
        let graph = dependencyGraph sheet
        assertEqual "A1 deps" Set.empty (lookupDeps (addr "A" 1) graph)
        assertEqual "A3 deps" (Set.fromList [addr "A" 1, addr "A" 2]) (lookupDeps (addr "A" 3) graph)
        assertEqual
            "A4 deps"
            (Set.fromList [addr "A" 1, addr "A" 2, addr "A" 3])
            (lookupDeps (addr "A" 4) graph)
    )

testEvaluationOrder :: Test
testEvaluationOrder =
    ( "acyclic sheet receives topological evaluation order"
    , do
        sheet <- parseOk sampleSheet
        assertEqual
            "order"
            (Right [addr "A" 1, addr "A" 2, addr "A" 3, addr "A" 4, addr "B" 1, addr "B" 2])
            (evaluationOrder sheet)
    )

testEndToEndEvaluation :: Test
testEndToEndEvaluation =
    ( "end-to-end sample sheet evaluates expected values"
    , do
        values <- evalOk sampleSheet
        assertEqual "A3" (Just (NumV 30)) (Map.lookup (addr "A" 3) values)
        assertEqual "A4" (Just (NumV 60)) (Map.lookup (addr "A" 4) values)
        assertEqual "B2" (Just (NumV 8)) (Map.lookup (addr "B" 2) values)
    )

testDivisionByZero :: Test
testDivisionByZero =
    ( "division by zero is a value error"
    , do
        values <- evalOk "sheet { A1 = 10 / 0; B1 = A1 + 3; }"
        assertEqual "A1" (Just (ErrV "division by zero")) (Map.lookup (addr "A" 1) values)
        assertEqual "B1" (Just (ErrV "division by zero")) (Map.lookup (addr "B" 1) values)
    )

testMissingReference :: Test
testMissingReference =
    ( "missing references become error values"
    , do
        values <- evalOk "sheet { A1 = Z9 + 2; }"
        assertEqual "A1" (Just (ErrV "missing cell Z9")) (Map.lookup (addr "A" 1) values)
    )

testTypeError :: Test
testTypeError =
    ( "non-numeric arithmetic becomes a type error"
    , do
        values <- evalOk "sheet { A1 = \"x\" + 2; }"
        assertEqual
            "A1"
            (Just (ErrV "type error: arithmetic expects numbers"))
            (Map.lookup (addr "A" 1) values)
    )

testCycleDetection :: Test
testCycleDetection =
    ( "cycle detection reports every cell in a cycle"
    , do
        sheet <- parseOk "sheet { A1 = B1; B1 = C1; C1 = A1; D1 = A1 + 1; }"
        assertEqual
            "cycle cells"
            (Set.fromList [addr "A" 1, addr "B" 1, addr "C" 1])
            (cycleCells sheet)
    )

testCycleDoesNotHang :: Test
testCycleDoesNotHang =
    ( "cycle evaluation terminates and propagates cycle errors"
    , do
        values <- evalOk "sheet { A1 = B1; B1 = A1; C1 = A1 + 1; }"
        assertEqual "A1" (Just (ErrV "cycle")) (Map.lookup (addr "A" 1) values)
        assertEqual "B1" (Just (ErrV "cycle")) (Map.lookup (addr "B" 1) values)
        assertEqual "C1" (Just (ErrV "cycle")) (Map.lookup (addr "C" 1) values)
    )

testRecomputeNoChangeInvariant :: Test
testRecomputeNoChangeInvariant =
    ( "property: recomputing the same acyclic sheet gives the same values"
    , forM_ acyclicSheets $ \source -> do
        sheet <- parseOk source
        assertEqual ("stable recompute for " ++ source) (evaluateSheet sheet) (evaluateSheet sheet)
    )

testOrderIndependenceInvariant :: Test
testOrderIndependenceInvariant =
    ( "property: acyclic values do not depend on assignment order"
    , do
        first <- evalOk "sheet { A1 = 1; A2 = A1 + 2; A3 = A2 * 3; }"
        second <- evalOk "sheet { A3 = A2 * 3; A2 = A1 + 2; A1 = 1; }"
        assertEqual "same values" first second
    )

testAffectedCells :: Test
testAffectedCells =
    ( "reactive dependency closure finds exactly transitive dependents"
    , do
        sheet <- parseOk "sheet { A1 = 1; A2 = A1 + 1; A3 = A2 + 1; B1 = 7; B2 = B1 + A3; C1 = 4; }"
        assertEqual
            "affected by A1"
            (Set.fromList [addr "A" 1, addr "A" 2, addr "A" 3, addr "B" 2])
            (affectedCells sheet (addr "A" 1))
    )

testReactiveMatchesFullEvaluation :: Test
testReactiveMatchesFullEvaluation =
    ( "reactive recalculation matches full evaluation"
    , do
        sheet <- parseOk "sheet { A1 = 1; A2 = A1 + 2; A3 = A2 * 3; B1 = 10; }"
        let oldValues = evaluateSheet sheet
        let result = recalculateAfterChange (addr "A" 1) (Lit (NumV 5)) sheet oldValues
        assertEqual
            "recalculated cells"
            (Set.fromList [addr "A" 1, addr "A" 2, addr "A" 3])
            (recalculatedCells result)
        assertEqual
            "same as full evaluation"
            (evaluateSheet (recalculatedSheet result))
            (recalculatedValues result)
    )

testReactiveLeavesUnrelatedValues :: Test
testReactiveLeavesUnrelatedValues =
    ( "reactive recalculation leaves unrelated cached values alone"
    , do
        sheet <- parseOk "sheet { A1 = 1; A2 = A1 + 1; B1 = 100; B2 = B1 + 1; C1 = A2 + B2; D1 = 7; }"
        let oldValues = evaluateSheet sheet
        let result = recalculateAfterChange (addr "B" 1) (Lit (NumV 200)) sheet oldValues
        let newValues = recalculatedValues result
        assertEqual
            "recalculated cells"
            (Set.fromList [addr "B" 1, addr "B" 2, addr "C" 1])
            (recalculatedCells result)
        forM_ [addr "A" 1, addr "A" 2, addr "D" 1] $ \cell ->
            assertEqual ("unchanged " ++ showAddr cell) (Map.lookup cell oldValues) (Map.lookup cell newValues)
    )

testReactiveCanCreateCycle :: Test
testReactiveCanCreateCycle =
    ( "reactive recalculation handles an update that creates a cycle"
    , do
        sheet <- parseOk "sheet { A1 = 1; A2 = A1 + 1; B1 = 5; }"
        let oldValues = evaluateSheet sheet
        let result = recalculateAfterChange (addr "A" 1) (Form (Ref (addr "A" 2))) sheet oldValues
        let newValues = recalculatedValues result
        assertEqual "A1" (Just (ErrV "cycle")) (Map.lookup (addr "A" 1) newValues)
        assertEqual "A2" (Just (ErrV "cycle")) (Map.lookup (addr "A" 2) newValues)
        assertEqual "B1" (Just (NumV 5)) (Map.lookup (addr "B" 1) newValues)
        assertEqual
            "same as full evaluation"
            (evaluateSheet (recalculatedSheet result))
            newValues
    )

sampleSheet :: String
sampleSheet =
    unlines
        [ "sheet {"
        , "  # base inputs"
        , "  A1 = 10;"
        , "  A2 = 20;"
        , "  A3 = A1 + A2;"
        , "  A4 = SUM(A1:A3);"
        , "  B1 = AVG(A1:A2);"
        , "  B2 = B1 - 7;"
        , "}"
        ]

acyclicSheets :: [String]
acyclicSheets =
    [ "sheet { A1 = 1; A2 = A1 + 1; }"
    , "sheet { A1 = 2; A2 = 3; A3 = A1 * A2 + 4; }"
    , "sheet { A1 = 2; A2 = 4; A3 = SUM(A1:A2); A4 = AVG(A1:A3); }"
    , "sheet { A1 = \"hello\"; A2 = true; A3 = 5; }"
    ]

parseOk :: String -> Either String Sheet
parseOk source =
    case parseSheet source of
        Right sheet -> Right sheet
        Left err -> Left ("parse failed: " ++ show err)

parseExprOk :: String -> Either String Expr
parseExprOk source =
    case parseExprText source of
        Right expr -> Right expr
        Left err -> Left ("parse failed: " ++ show err)

evalOk :: String -> Either String (Map Addr Value)
evalOk source = evaluateSheet <$> parseOk source

lookupDeps :: Addr -> Map Addr (Set.Set Addr) -> Set.Set Addr
lookupDeps cell graph = Map.findWithDefault Set.empty cell graph

addr :: String -> Int -> Addr
addr = mkAddr

assertEqual :: (Eq a, Show a) => String -> a -> a -> Either String ()
assertEqual label expected actual
    | expected == actual = Right ()
    | otherwise =
        Left
            ( label
                ++ ": expected "
                ++ show expected
                ++ ", got "
                ++ show actual
            )
