module SpreadsheetLang where

import Control.Applicative (Alternative(..), optional)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, ord, toLower, toUpper)
import Data.Graph (SCC(..), stronglyConnComp)
import Data.List (foldl', intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Sheet = Sheet
    { sheetCells :: Map Addr Content
    } deriving (Eq, Show)

data Cell = Cell
    { cellAddr :: Addr
    , cellContent :: Content
    } deriving (Eq, Show)

data Addr = Addr String Int
    deriving (Eq, Ord)

instance Show Addr where
    show = showAddr

data Content
    = Lit Value
    | Form Expr
    deriving (Eq, Show)

data Expr
    = Ref Addr
    | LitE Value
    | BinOp Op Expr Expr
    | RangeOp RangeOp Addr Addr
    deriving (Eq, Show)

data Value
    = NumV Double
    | BoolV Bool
    | StrV String
    | ErrV String
    deriving (Eq, Show)

data Op = Add | Sub | Mul | Div
    deriving (Eq, Show)

data RangeOp = SumR | AvgR
    deriving (Eq, Show)

data ParseError = ParseError
    { errorLine :: Int
    , errorColumn :: Int
    , errorMessage :: String
    } deriving (Eq)

instance Show ParseError where
    show err =
        "line " ++ show (errorLine err)
            ++ ", column " ++ show (errorColumn err)
            ++ ": " ++ errorMessage err

mkAddr :: String -> Int -> Addr
mkAddr col row = Addr (map toUpper col) row

showAddr :: Addr -> String
showAddr (Addr col row) = col ++ show row

fromCells :: [Cell] -> Either String Sheet
fromCells = fmap Sheet . foldr insertCell (Right Map.empty)
  where
    insertCell cell acc = do
        cells <- acc
        let addr = cellAddr cell
        if Map.member addr cells
            then Left ("duplicate cell " ++ showAddr addr)
            else Right (Map.insert addr (cellContent cell) cells)

parseSheet :: String -> Either ParseError Sheet
parseSheet input = do
    (sheet, _state) <- runParser (skipIgnored *> parseSheetBody <* eof) initialState
    pure sheet
  where
    initialState = PState input 1 1

parseExprText :: String -> Either ParseError Expr
parseExprText input = do
    (expr, _state) <- runParser (skipIgnored *> parseExpr <* eof) (PState input 1 1)
    pure expr

data PState = PState
    { psInput :: String
    , psLine :: Int
    , psColumn :: Int
    } deriving (Eq, Show)

newtype Parser a = Parser
    { runParser :: PState -> Either ParseError (a, PState)
    }

instance Functor Parser where
    fmap f parser = Parser $ \state -> do
        (value, nextState) <- runParser parser state
        pure (f value, nextState)

instance Applicative Parser where
    pure value = Parser $ \state -> Right (value, state)

    functionParser <*> valueParser = Parser $ \state -> do
        (function, state') <- runParser functionParser state
        (value, state'') <- runParser valueParser state'
        pure (function value, state'')

instance Monad Parser where
    parser >>= next = Parser $ \state -> do
        (value, state') <- runParser parser state
        runParser (next value) state'

instance Alternative Parser where
    empty = parserFail "unexpected input"

    first <|> second = Parser $ \state ->
        case runParser first state of
            Right result -> Right result
            Left _ -> runParser second state

parserFail :: String -> Parser a
parserFail message = Parser $ \state ->
    Left (ParseError (psLine state) (psColumn state) message)

eof :: Parser ()
eof = Parser $ \state ->
    case psInput state of
        [] -> Right ((), state)
        _ -> Left (ParseError (psLine state) (psColumn state) "expected end of input")

peekChar :: Parser (Maybe Char)
peekChar = Parser $ \state ->
    Right
        ( case psInput state of
            [] -> Nothing
            c:_ -> Just c
        , state
        )

item :: Parser Char
item = Parser $ \state ->
    case psInput state of
        [] -> Left (ParseError (psLine state) (psColumn state) "unexpected end of input")
        c:rest -> Right (c, advance c state { psInput = rest })

advance :: Char -> PState -> PState
advance '\n' state = state { psLine = psLine state + 1, psColumn = 1 }
advance _ state = state { psColumn = psColumn state + 1 }

satisfy :: (Char -> Bool) -> String -> Parser Char
satisfy predicate expected = do
    c <- item
    if predicate c
        then pure c
        else parserFail ("expected " ++ expected)

charP :: Char -> Parser Char
charP expected = satisfy (== expected) (show [expected])

stringP :: String -> Parser String
stringP = traverse charP

notFollowedBy :: Parser a -> String -> Parser ()
notFollowedBy parser message = Parser $ \state ->
    case runParser parser state of
        Right _ -> Left (ParseError (psLine state) (psColumn state) message)
        Left _ -> Right ((), state)

skipIgnored :: Parser ()
skipIgnored = do
    skipped <- skipOne
    case skipped of
        True -> skipIgnored
        False -> pure ()
  where
    skipOne =
        skipWhitespace
            <|> skipLineComment "--"
            <|> skipLineComment "//"
            <|> skipLineComment "#"
            <|> pure False

    skipWhitespace = do
        _ <- satisfy isSpace "whitespace"
        pure True

    skipLineComment marker = do
        _ <- stringP marker
        _ <- many (satisfy (/= '\n') "comment character")
        _ <- optional (charP '\n')
        pure True

lexeme :: Parser a -> Parser a
lexeme parser = skipIgnored *> parser <* skipIgnored

symbol :: String -> Parser String
symbol text = lexeme (stringP text)

keyword :: String -> Parser String
keyword text = lexeme $ do
    parsed <- traverse charCI text
    notFollowedBy (satisfy isAlphaNum "identifier character") ("expected end of keyword " ++ text)
    pure parsed

charCI :: Char -> Parser Char
charCI expected =
    satisfy (\c -> toLower c == toLower expected) (show [expected])

parseSheetBody :: Parser Sheet
parseSheetBody = do
    _ <- keyword "sheet"
    _ <- symbol "{"
    parsedCells <- parseCells
    case fromCells parsedCells of
        Right sheet -> pure sheet
        Left message -> parserFail message

parseCells :: Parser [Cell]
parseCells =
    (symbol "}" *> pure [])
        <|> ((:) <$> parseCell <*> parseCells)

parseCell :: Parser Cell
parseCell = do
    parsedAddr <- lexeme parseAddr
    _ <- symbol "="
    parsedExpr <- parseExpr
    _ <- symbol ";"
    pure (Cell parsedAddr (exprToContent parsedExpr))

exprToContent :: Expr -> Content
exprToContent (LitE value) = Lit value
exprToContent expr = Form expr

parseExpr :: Parser Expr
parseExpr = parseAddSub

parseAddSub :: Parser Expr
parseAddSub = chainLeft parseMulDiv parseAddSubOp

parseMulDiv :: Parser Expr
parseMulDiv = chainLeft parseFactor parseMulDivOp

parseAddSubOp :: Parser (Expr -> Expr -> Expr)
parseAddSubOp =
    (symbol "+" *> pure (BinOp Add))
        <|> (symbol "-" *> pure (BinOp Sub))

parseMulDivOp :: Parser (Expr -> Expr -> Expr)
parseMulDivOp =
    (symbol "*" *> pure (BinOp Mul))
        <|> (symbol "/" *> pure (BinOp Div))

chainLeft :: Parser a -> Parser (a -> a -> a) -> Parser a
chainLeft itemParser opParser = do
    first <- itemParser
    rest first
  where
    rest left =
        (do
            op <- opParser
            right <- itemParser
            rest (op left right))
            <|> pure left

parseFactor :: Parser Expr
parseFactor =
    parseParens
        <|> parseRangeExpr
        <|> parseStringExpr
        <|> parseBoolExpr
        <|> parseNumberExpr
        <|> (Ref <$> lexeme parseAddr)

parseParens :: Parser Expr
parseParens = do
    _ <- symbol "("
    expr <- parseExpr
    _ <- symbol ")"
    pure expr

parseRangeExpr :: Parser Expr
parseRangeExpr = lexeme $ do
    name <- parseWord
    op <- case map toUpper name of
        "SUM" -> pure SumR
        "AVG" -> pure AvgR
        other -> parserFail ("unknown range operation " ++ other)
    skipIgnored
    _ <- charP '('
    skipIgnored
    start <- parseAddr
    skipIgnored
    _ <- charP ':'
    skipIgnored
    end <- parseAddr
    skipIgnored
    _ <- charP ')'
    pure (RangeOp op start end)

parseStringExpr :: Parser Expr
parseStringExpr = LitE . StrV <$> lexeme parseString

parseString :: Parser String
parseString = do
    _ <- charP '"'
    chars <- many parseStringChar
    _ <- charP '"'
    pure chars

parseStringChar :: Parser Char
parseStringChar =
    parseEscaped
        <|> satisfy (\c -> c /= '"' && c /= '\n') "string character"
  where
    parseEscaped = do
        _ <- charP '\\'
        escaped <- item
        case escaped of
            '"' -> pure '"'
            '\\' -> pure '\\'
            'n' -> pure '\n'
            't' -> pure '\t'
            other -> parserFail ("unknown escape sequence \\" ++ [other])

parseBoolExpr :: Parser Expr
parseBoolExpr = lexeme $ do
    word <- parseWord
    case map toLower word of
        "true" -> pure (LitE (BoolV True))
        "false" -> pure (LitE (BoolV False))
        _ -> parserFail "expected boolean literal"

parseNumberExpr :: Parser Expr
parseNumberExpr = LitE . NumV <$> lexeme parseNumber

parseNumber :: Parser Double
parseNumber = do
    sign <- optional (charP '-')
    whole <- some (satisfy isDigit "digit")
    fraction <- optional parseFraction
    let signText = maybe "" (: []) sign
    pure (read (signText ++ whole ++ maybe "" id fraction))

parseFraction :: Parser String
parseFraction = do
    dot <- charP '.'
    digits <- some (satisfy isDigit "digit")
    pure (dot : digits)

parseAddr :: Parser Addr
parseAddr = do
    col <- some (satisfy isAlpha "column letter")
    rowText <- some (satisfy isDigit "row number")
    let row = read rowText
    if row <= 0
        then parserFail "row number must be positive"
        else pure (mkAddr col row)

parseWord :: Parser String
parseWord = some (satisfy isAlpha "letter")

sheetFromList :: [(Addr, Content)] -> Sheet
sheetFromList = Sheet . Map.fromList

lookupCell :: Addr -> Sheet -> Maybe Content
lookupCell addr (Sheet cells) = Map.lookup addr cells

formatValues :: Map Addr Value -> String
formatValues values =
    intercalate "\n"
        [ showAddr addr ++ " = " ++ show value
        | (addr, value) <- Map.toAscList values
        ]

dependencies :: Content -> Set Addr
dependencies (Lit _) = Set.empty
dependencies (Form expr) = exprDependencies expr

exprDependencies :: Expr -> Set Addr
exprDependencies (LitE _) = Set.empty
exprDependencies (Ref addr) = Set.singleton addr
exprDependencies (BinOp _ left right) =
    Set.union (exprDependencies left) (exprDependencies right)
exprDependencies (RangeOp _ start end) =
    Set.fromList (rangeAddresses start end)

dependencyGraph :: Sheet -> Map Addr (Set Addr)
dependencyGraph (Sheet cells) = Map.map dependencies cells

dependenciesInSheet :: Sheet -> Map Addr (Set Addr)
dependenciesInSheet sheet@(Sheet cells) =
    Map.map (Set.filter (`Map.member` cells)) (dependencyGraph sheet)

dependentsGraph :: Sheet -> Map Addr (Set Addr)
dependentsGraph sheet =
    Map.foldlWithKey' addDependents emptyGraph (dependencyGraph sheet)
  where
    emptyGraph = Map.fromSet (const Set.empty) (Map.keysSet (sheetCells sheet))

    addDependents graph addr deps =
        Set.foldl' (\acc dep -> Map.insertWith Set.union dep (Set.singleton addr) acc) graph deps

cycleCells :: Sheet -> Set Addr
cycleCells sheet =
    Set.fromList (concatMap fromComponent components)
  where
    directDeps = dependenciesInSheet sheet
    components =
        stronglyConnComp
            [ (addr, addr, Set.toList deps)
            | (addr, deps) <- Map.toAscList directDeps
            ]

    fromComponent (CyclicSCC addrs) = addrs
    fromComponent (AcyclicSCC addr)
        | Set.member addr (Map.findWithDefault Set.empty addr directDeps) = [addr]
        | otherwise = []

evaluationOrder :: Sheet -> Either [Addr] [Addr]
evaluationOrder sheet
    | Set.null cycles = Right (topologicalOrderIgnoring sheet Set.empty)
    | otherwise = Left (Set.toAscList cycles)
  where
    cycles = cycleCells sheet

evaluateText :: String -> Either ParseError (Map Addr Value)
evaluateText input = evaluateSheet <$> parseSheet input

evaluateSheet :: Sheet -> Map Addr Value
evaluateSheet sheet@(Sheet cells) =
    foldl' evaluateOne initialValues order
  where
    cycles = cycleCells sheet
    initialValues = Map.fromSet (const (ErrV "cycle")) cycles
    order = topologicalOrderIgnoring sheet cycles

    evaluateOne values addr =
        case Map.lookup addr cells of
            Nothing -> values
            Just content -> Map.insert addr (evalContent sheet values content) values

evalContent :: Sheet -> Map Addr Value -> Content -> Value
evalContent _ _ (Lit value) = value
evalContent sheet values (Form expr) = evalExpr sheet values expr

evalExpr :: Sheet -> Map Addr Value -> Expr -> Value
evalExpr _ _ (LitE value) = value
evalExpr sheet values (Ref addr)
    | Map.member addr (sheetCells sheet) =
        Map.findWithDefault (ErrV ("unresolved cell " ++ showAddr addr)) addr values
    | otherwise = ErrV ("missing cell " ++ showAddr addr)
evalExpr sheet values (BinOp op left right) =
    applyOp op (evalExpr sheet values left) (evalExpr sheet values right)
evalExpr sheet values (RangeOp op start end) =
    applyRangeOp op (map (evalExpr sheet values . Ref) (rangeAddresses start end))

applyOp :: Op -> Value -> Value -> Value
applyOp _ (ErrV message) _ = ErrV message
applyOp _ _ (ErrV message) = ErrV message
applyOp Add (NumV left) (NumV right) = NumV (left + right)
applyOp Sub (NumV left) (NumV right) = NumV (left - right)
applyOp Mul (NumV left) (NumV right) = NumV (left * right)
applyOp Div (NumV _) (NumV 0) = ErrV "division by zero"
applyOp Div (NumV left) (NumV right) = NumV (left / right)
applyOp _ _ _ = ErrV "type error: arithmetic expects numbers"

applyRangeOp :: RangeOp -> [Value] -> Value
applyRangeOp op values =
    case collectNumbers values of
        Left err -> err
        Right nums -> evalRangeNumbers op nums

collectNumbers :: [Value] -> Either Value [Double]
collectNumbers = foldr collectOne (Right [])
  where
    collectOne (NumV number) (Right numbers) = Right (number : numbers)
    collectOne (ErrV message) _ = Left (ErrV message)
    collectOne _ _ = Left (ErrV "type error: range expects numbers")

evalRangeNumbers :: RangeOp -> [Double] -> Value
evalRangeNumbers SumR nums = NumV (sum nums)
evalRangeNumbers AvgR [] = ErrV "empty range"
evalRangeNumbers AvgR nums = NumV (sum nums / fromIntegral (length nums))

topologicalOrderIgnoring :: Sheet -> Set Addr -> [Addr]
topologicalOrderIgnoring sheet@(Sheet cells) ignored =
    go initialReady initialIndegrees []
  where
    activeCells = Map.keysSet cells `Set.difference` ignored
    activeDeps =
        Map.map
            (Set.filter (`Set.member` activeCells))
            (Map.restrictKeys (dependenciesInSheet sheet) activeCells)
    initialIndegrees = Map.map Set.size activeDeps
    initialReady =
        Map.keysSet (Map.filter (== 0) initialIndegrees)
    reverseGraph =
        Map.foldlWithKey' addReverseDeps (Map.fromSet (const Set.empty) activeCells) activeDeps

    addReverseDeps graph addr deps =
        Set.foldl' (\acc dep -> Map.insertWith Set.union dep (Set.singleton addr) acc) graph deps

    go ready indegrees ordered
        | Set.null ready = reverse ordered
        | otherwise =
            let (addr, restReady) = Set.deleteFindMin ready
                dependents = Map.findWithDefault Set.empty addr reverseGraph
                (nextReady, nextIndegrees) =
                    Set.foldl'
                        releaseDependent
                        (restReady, Map.delete addr indegrees)
                        dependents
            in go nextReady nextIndegrees (addr : ordered)

    releaseDependent (ready, indegrees) addr =
        case Map.lookup addr indegrees of
            Nothing -> (ready, indegrees)
            Just count ->
                let nextCount = count - 1
                    nextIndegrees = Map.insert addr nextCount indegrees
                    nextReady =
                        if nextCount == 0
                            then Set.insert addr ready
                            else ready
                in (nextReady, nextIndegrees)

rangeAddresses :: Addr -> Addr -> [Addr]
rangeAddresses (Addr startCol startRow) (Addr endCol endRow) =
    [ Addr (columnName col) row
    | col <- [min startColNumber endColNumber .. max startColNumber endColNumber]
    , row <- [min startRow endRow .. max startRow endRow]
    ]
  where
    startColNumber = columnNumber startCol
    endColNumber = columnNumber endCol

columnNumber :: String -> Int
columnNumber =
    foldl' (\acc c -> acc * 26 + alphaOffset (toUpper c)) 0
  where
    alphaOffset c = ord c - ord 'A' + 1

columnName :: Int -> String
columnName number
    | number <= 0 = ""
    | otherwise = reverse (go number)
  where
    go 0 = []
    go n =
        let (q, r) = (n - 1) `quotRem` 26
            letter = toEnum (ord 'A' + r)
        in letter : go q
