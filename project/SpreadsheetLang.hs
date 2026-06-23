module SpreadsheetLang where

import Control.Applicative (Alternative(..), optional)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toLower, toUpper)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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
