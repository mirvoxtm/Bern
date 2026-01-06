module Parsing.Parser where

import Language.Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void String

-- TODO: change to a whitespace type language like python

-- Space consumer to handle whitespace and comments
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
    where
        lineCmnt  = L.skipLineComment "//" <|> L.skipLineComment "--"
        blockCmnt = L.skipBlockComment "/*" "*/" <|> L.skipBlockComment "{-" "-}"

-- Lexeme parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Symbol parser
symbol :: String -> Parser String
symbol = L.symbol sc

-- Parser for numbers
parseNumber :: Parser Expression
parseNumber = lexeme $ do
    n <- L.signed sc L.decimal
    return $ Number n

-- Parser for variables
parseVariable :: Parser Expression
parseVariable = lexeme $ do
    varName <- some letterChar
    return $ Variable varName

-- Parser for Boolean literals
parseBoolean :: Parser Expression
parseBoolean = lexeme $ (string "true" >> return (BoolLiteral True))
                     <|> (string "false" >> return (BoolLiteral False))

-- Parser for parentheses
parseParens :: Parser Expression
parseParens = between (symbol "(") (symbol ")") parseExpression

-- Parse binary operations
parseBinaryOp :: Parser Expression
parseBinaryOp = do
    left <- parseTerm
    op <- lexeme $ choice
        [ string "+" >> return Add
        , string "-" >> return Subtract
        , string "*" >> return Multiply
        , string "/" >> return Divide
        , string "==" >> return Equal
        , string "!=" >> return Different
        , string ">" >> return GreaterThan
        , string "<" >> return LessThan
        , string ">=" >> return GreaterThanEq
        , string "<=" >> return LessThanEq
        ]
    right <- parseTerm
    return $ BinaryOperator op left right

-- Parse unary operations
parseUnaryOp :: Parser Expression
parseUnaryOp = do
    op <- lexeme $ choice
        [ string "-" >> return Negate
        , string "!" >> return Not
        ]
    expr <- parseTerm
    return $ UnaryOperator op expr

-- Parse terms (numbers, variables, parentheses)
parseTerm :: Parser Expression
parseTerm = try parseNumber
    <|> try parseVariable
    <|> try parseBoolean
    <|> try parseUnaryOp
    <|> parseParens
    <|> parseBinaryOp

parseExpression :: Parser Expression
parseExpression = try parseBinaryOp
              <|> try parseUnaryOp
              <|> parseTerm

parseAssignment :: Parser Command
parseAssignment = do
    varName <- lexeme $ some letterChar
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
    thenCmd <- parseCommand
    _ <- symbol "else"
    elseCmd <- parseCommand
    _ <- symbol "end"
    return $ Conditional cond thenCmd elseCmd

parseRepeat :: Parser Command
parseRepeat = do
    _ <- symbol "for"
    count <- parseExpression
    _ <- symbol "do"
    cmd <- parseCommand
    _ <- symbol "end"
    return $ Repeat count cmd

parseWhile :: Parser Command
parseWhile = do
    _ <- symbol "while"
    cond <- parseExpression
    _ <- symbol "do"
    cmd <- parseCommand
    _ <- symbol "end"
    return $ While cond cmd

parseConcat :: Parser Command
parseConcat = do
    cmd1 <- parseCommand
    _ <- some (char '\n' <|> char '\r')
    cmd2 <- parseCommand
    return $ Concat cmd1 cmd2

parseSingleCommand :: Parser Command
parseSingleCommand = try parseAssignment
               <|> try parsePrint
               <|> try parseConditional
               <|> try parseRepeat
               <|> try parseWhile
               <|> (Print <$> parseExpression)

parseCommand :: Parser Command
parseCommand = parseSingleCommand

-- Top-level parser
parseBern :: String -> Either (ParseErrorBundle String Void) Command
parseBern input = parse (sc >> parseCommand <* eof) "" input