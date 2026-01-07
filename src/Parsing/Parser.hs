module Parsing.Parser where

import Language.Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void
import Control.Monad (void)

type Parser = Parsec Void String

-- Space consumer that consumes spaces, tabs, and comments but NOT newlines
scn :: Parser ()
scn = L.space (void $ some (char ' ' <|> char '\t')) lineCmnt blockCmnt
    where
        lineCmnt  = L.skipLineComment "//" <|> L.skipLineComment "--"
        blockCmnt = L.skipBlockComment "/*" "*/" <|> L.skipBlockComment "{-" "-}"

-- Space consumer that also consumes newlines (for inside blocks)
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
    where
        lineCmnt  = L.skipLineComment "//" <|> L.skipLineComment "--"
        blockCmnt = L.skipBlockComment "/*" "*/" <|> L.skipBlockComment "{-" "-}"

-- Lexeme parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

-- Symbol parser
symbol :: String -> Parser String
symbol = L.symbol sc

-- Parser for numbers
parseNumber :: Parser Expression
parseNumber = lexeme $ do
    n <- L.signed sc L.decimal
    return $ Number n

parseDouble :: Parser Expression
parseDouble = lexeme $ do
    d <- L.signed sc L.float
    return $ DoubleNum d

-- Parser for variables
parseVariable :: Parser Expression
parseVariable = lexeme $ do
    firstChar <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    return $ Variable (firstChar : rest)

-- Parser for Boolean literals
parseBoolean :: Parser Expression
parseBoolean = lexeme $ (string "true" >> return (BoolLiteral True))
                     <|> (string "false" >> return (BoolLiteral False))

-- Parser for string literals
parseString :: Parser Expression
parseString = lexeme $ do
    _ <- char '"'
    content <- manyTill L.charLiteral (char '"')
    return $ StringLiteral content

-- Parser for character literals
parseChar :: Parser Expression
parseChar = lexeme $ do
    _ <- char '\''
    c <- L.charLiteral
    _ <- char '\''
    return $ CharLiteral c

-- Parser for list literals [elem1, elem2, ...]
parseList :: Parser Expression
parseList = do
    _ <- symbol "["
    -- Try range syntax [start .. end]; fall back to comma-separated list
    range <- optional $ try $ do
        start <- parseExpression
        _ <- symbol ".."
        end <- parseExpression
        return (start, end)
    case range of
        Just (start, end) -> do
            _ <- symbol "]"
            return $ Range start end
        Nothing -> do
            elems <- parseExpression `sepBy` symbol ","
            _ <- symbol "]"
            return $ ListLiteral elems

-- Parser for set literals {elem1, elem2, ...}
parseSet :: Parser Expression
parseSet = do
    _ <- symbol "{"
    elems <- parseExpression `sepBy` symbol ","
    _ <- symbol "}"
    return $ SetLiteral elems

-- Parser for parentheses
parseParens :: Parser Expression
parseParens = between (symbol "(") (symbol ")") parseExpression

-- Parse binary operations
binary :: String -> BinaryOperation -> Operator Parser Expression
binary name f = InfixL (BinaryOperator f <$ symbol name)

prefix :: String -> UnaryOperation -> Operator Parser Expression
prefix name f = Prefix (UnaryOperator f <$ symbol name)

operatorTable :: [[Operator Parser Expression]]
operatorTable =
        [ [ prefix "-" Negate
            , prefix "!" Not
            , prefix "::" TypeOf
            , prefix ":>" SizeOf
            ]
        , [ binary "*" Multiply
            , binary "/" Divide
            , binary "%" Modulo
            ]
        , [ binary "+" Add
            , binary "-" Subtract
            ]
        , [ binary "<>" Concatenation
            , binary "<|" Union
            , binary "|>" Intersection
            , binary "</>" Difference
            ]
        , [ binary ">=" GreaterThanEq
            , binary "<=" LessThanEq
            , binary ">" GreaterThan
            , binary "<" LessThan
            ]
        , [ binary "==" Equal
            , binary "!=" Different
            , binary "is" Equal
            ]
        , [ binary "&&" And ]
        , [ binary "||" Or ]
        ]

-- Parse terms (numbers, variables, parentheses)
parseTerm :: Parser Expression
parseTerm = do
    base <- parseBaseTerm
    indices <- many parseIndexAccess
    return $ foldl Index base indices

-- Parse base terms without index access
parseBaseTerm :: Parser Expression
parseBaseTerm = try parseDouble
    <|> try parseNumber
    <|> try parseString
    <|> try parseChar
    <|> try parseBoolean
    <|> try parseList
    <|> try parseSet
    <|> try parseVariable
    <|> parseParens

-- Parse index access [expr]
parseIndexAccess :: Parser Expression
parseIndexAccess = do
    _ <- char '['
    idx <- parseExpression
    _ <- char ']'
    return idx

parseExpression :: Parser Expression
parseExpression = makeExprParser (lexeme parseTerm) operatorTable

parseVarWithIndices :: Parser (String, [Expression])
parseVarWithIndices = do
    firstChar <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    let varName = firstChar : rest
    idxs <- many parseIndexAccess
    return (varName, idxs)

parseAssignment :: Parser Command
parseAssignment = try parseIndexedAssign <|> parseSimpleAssign
  where
    parseIndexedAssign = try $ do
        (varName, idxs) <- lexeme parseVarWithIndices
        if null idxs then fail "no indices" else pure ()
        _ <- scn
        _ <- symbol "="
        expr <- parseExpression
        return $ AssignIndex varName idxs expr
    parseSimpleAssign = try $ do
        (varName, idxs) <- lexeme parseVarWithIndices
        if not (null idxs) then fail "indexed" else pure ()
        _ <- scn
        _ <- symbol "="
        expr <- parseExpression
        return $ Assign varName expr

parsePrint :: Parser Command
parsePrint = do
    _ <- symbol "print"
    expr <- parseExpression
    return $ Print expr

parseConditional :: Parser Command
parseConditional = do
    _ <- symbol "if"
    cond <- parseExpression
    _ <- symbol "then"
    thenCmd <- parseBlockCommands
    elseCmd <- parseElseChain
    return $ Conditional cond thenCmd elseCmd

-- Handles "else if" chains ending with an "else" (or no else -> Skip)
parseElseChain :: Parser Command
parseElseChain = try parseElseIf <|> parseElseOnly <|> parseNoElse
  where
    parseElseIf = do
        _ <- symbol "else"
        _ <- symbol "if"
        cond <- parseExpression
        _ <- symbol "then"
        thenCmd <- parseBlockCommands
        elseCmd <- parseElseChain
        return $ Conditional cond thenCmd elseCmd

    parseElseOnly = do
        _ <- symbol "else"
        elseCmd <- parseBlockCommands
        _ <- symbol "end"
        return elseCmd

    parseNoElse = do
        _ <- symbol "end"
        return Skip

parseRepeat :: Parser Command
parseRepeat = do
    _ <- symbol "for"
    countExpr <- parseExpression
    _ <- symbol "do"
    cmd <- parseBlockCommands
    _ <- symbol "end"
    -- If it's a simple number, use Repeat; otherwise use While (for conditions)
    case countExpr of
        Number _ -> return $ Repeat countExpr cmd
        _        -> return $ While countExpr cmd

parseForIn :: Parser Command
parseForIn = do
    _ <- symbol "for"
    varName <- lexeme $ some letterChar
    idxVar <- optional (symbol "," >> lexeme (some letterChar))
    _ <- symbol ":"
    collection <- parseExpression
    _ <- symbol "do"
    body <- parseBlockCommands
    _ <- symbol "end"
    case idxVar of
        Just i  -> return $ ForInCount varName i collection body
        Nothing -> return $ ForIn varName collection body

parseWhile :: Parser Command
parseWhile = do
    _ <- symbol "while"
    cond <- parseExpression
    _ <- symbol "do"
    cmd <- parseBlockCommands
    _ <- symbol "end"
    return $ While cond cmd

parseSingleCommand :: Parser Command
parseSingleCommand = try parsePrint
               <|> try parseConditional
               <|> try parseForIn
               <|> try parseRepeat
               <|> try parseWhile
               <|> try parseAssignment
               <|> (Print <$> parseExpression)

-- Parse commands inside a block (stops at end/else keywords)
parseBlockCommands :: Parser Command
parseBlockCommands = do
    sc  -- consume all whitespace including newlines
    cmd <- parseSingleCommand
    rest <- many $ try $ do
        sc
        notFollowedBy (string "end" <|> string "else")
        parseSingleCommand
    sc
    return $ foldl Concat cmd rest

-- Parse multiple top-level commands  
parseCommands :: Parser Command
parseCommands = do
    sc
    cmd <- parseSingleCommand
    rest <- many $ try $ do
        sc
        notFollowedBy eof
        parseSingleCommand
    sc
    return $ foldl Concat cmd rest

parseCommand :: Parser Command
parseCommand = parseCommands

-- Top-level parser
parseBern :: String -> Either (ParseErrorBundle String Void) Command
parseBern input = parse (sc *> parseCommand <* sc <* eof) "" input

-- Parse a file (multiple lines)
parseBernFile :: String -> Either (ParseErrorBundle String Void) Command
parseBernFile = parseBern