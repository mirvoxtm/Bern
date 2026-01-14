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

-- Symbol parser (consumes all whitespace including newlines)
symbol :: String -> Parser String
symbol = L.symbol sc

-- Symbol parser that does NOT consume newlines (for operators in expressions)
symbolNoNl :: String -> Parser String
symbolNoNl = L.symbol scn

-- Keyword parser (ensures keyword is not followed by alphanumeric/underscore)
keyword :: String -> Parser String
keyword kw = lexeme $ try $ do
    _ <- string kw
    notFollowedBy (alphaNumChar <|> char '_')
    return kw

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

-- Key/value pairs for object literals. Uses the newline-tolerant space consumer
-- so objects can be written across multiple lines.
parseKeyValuePair :: Parser (String, Expression)
parseKeyValuePair = do
    key <- L.lexeme sc parseKey
    _ <- symbol ":"
    value <- parseExpression
    return (key, value)
  where
    parseKey = parseStringKey <|> parseIdentKey
    -- Allow quoted string keys: "key"
    parseStringKey = do
        _ <- char '"'
        content <- manyTill L.charLiteral (char '"')
        return content
    -- Or bare identifiers/digits: foo, foo1
    parseIdentKey = some (letterChar <|> char '_' <|> digitChar)

-- Parser for object literals #{ key: value, ... }#
parseObject :: Parser Expression
parseObject = try parseHashBraces <|> parseBareHash
    where
        parseHashBraces = do
            _ <- symbol "#{"
            pairs <- parseKeyValuePair `sepBy` symbol ","
            -- Allow closing after any intervening whitespace/newlines
            _ <- sc *> symbol "}#"
            return $ ObjectLiteral pairs
        parseBareHash = do
            _ <- lexeme (char '#')
            pairs <- parseKeyValuePair `sepBy` symbol ","
            -- Allow closing after any intervening whitespace/newlines
            _ <- sc *> symbol "#"
            return $ ObjectLiteral pairs

-- Parser for list literals [elem1, elem2, ...]
parseList :: Parser Expression
parseList = do
    _ <- symbolNoNl "["
    -- Try range syntax [start .. end]; fall back to comma-separated list
    range <- optional $ try $ do
        start <- parseExpression
        _ <- symbolNoNl ".."
        end <- parseExpression
        return (start, end)
    case range of
        Just (start, end) -> do
            _ <- symbolNoNl "]"
            return $ Range start end
        Nothing -> do
            elems <- parseExpression `sepBy` symbolNoNl ","
            _ <- symbolNoNl "]"
            return $ ListLiteral elems

-- Parser for set literals {elem1, elem2, ...}
parseSet :: Parser Expression
parseSet = do
    _ <- symbolNoNl "{"
    elems <- parseExpression `sepBy` symbolNoNl ","
    _ <- symbolNoNl "}"
    return $ SetLiteral elems

-- Parser for parentheses
parseParens :: Parser Expression
parseParens = between (symbolNoNl "(") (symbolNoNl ")") parseExpression

-- Parse binary operations
binary :: String -> BinaryOperation -> Operator Parser Expression
binary name f = InfixL (BinaryOperator f <$ symbolNoNl name)

prefix :: String -> UnaryOperation -> Operator Parser Expression
prefix name f = Prefix (UnaryOperator f <$ symbolNoNl name)

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
    pos <- getSourcePos
    base <- parseBaseTerm
    indices <- many parseIndexAccess
    return $ WithPos pos (foldl Index base indices)

-- Parse base terms without index access
parseBaseTerm :: Parser Expression
parseBaseTerm = try parseObject
    <|> try parseLambda
    <|> try parseDouble
    <|> try parseNumber
    <|> try parseString
    <|> try parseChar
    <|> try parseBoolean
    <|> try parseList
    <|> try parseSet
    <|> try parseReadFile
    <|> try parseFmap
    <|> try parseFunctionCallOrVar
    <|> parseParens

parseCBinding ::  Parser Command
parseCBinding = do
    _ <- symbol "foreign"
    
    -- Parse the function name
    funcName <- lexeme $ do
        first <- letterChar <|> char '_'
        rest <- many (alphaNumChar <|> char '_')
        return (first :  rest)
    
    -- Parse the library path (the .so/.dll file)
    _ <- symbolNoNl "("
    libPathExpr <- parseExpression
    mArgs <- optional $ do
        _ <- symbolNoNl ","
        parseTypeString `sepBy1` symbolNoNl ","
    _ <- symbolNoNl ")"

    -- Arrow indicating return type
    _ <- symbolNoNl "->"
    retType <- parseTypeString

    let argTypes = maybe [] id mArgs
    -- Return a command that loads the C function
    return $ CForeignDecl funcName libPathExpr argTypes retType
  where
    parseStringLiteral ::  Parser String
    parseStringLiteral = lexeme $ do
        _ <- char '"'
        content <- manyTill L.charLiteral (char '"')
        return content
    
    parseTypeString :: Parser String
    parseTypeString = lexeme $ do
        _ <- char '"'
        typeStr <- some (letterChar <|> char '_')
        _ <- char '"'
        return typeStr

-- Parse a variable or a function call (name(args...))
parseFunctionCallOrVar :: Parser Expression
parseFunctionCallOrVar = lexeme $ do
    firstChar <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    let name = firstChar : rest
    margs <- optional $ between (symbolNoNl "(") (symbolNoNl ")") (parseExpression `sepBy` symbolNoNl ",")
    case margs of
        Just args -> return $ FunctionCall name args
        Nothing   -> return $ Variable name

-- Parse a lambda: either \x,y -> expr or (x,y) -> expr
parseLambda :: Parser Expression
parseLambda = try backslashLambda <|> try parenLambda
  where
    backslashLambda = do
        _ <- char '\\'
        params <- parsePatterns
        _ <- symbol "->"
        body <- parseExpression
        return $ LambdaExpr params body
    parenLambda = do
        _ <- symbol "("
        params <- parsePatterns
        _ <- symbol ")"
        _ <- symbol "->"
        body <- parseExpression
        return $ LambdaExpr params body

parsePatterns :: Parser [Pattern]
parsePatterns = parsePattern `sepBy` symbol ","

parsePattern :: Parser Pattern
parsePattern = lexeme $ choice
    [ string "_" >> return PWildcard
    , try parseADTConstructorPattern
    , try parseListPattern  -- [head|tail] or [] or [a, b, c]
    , try parseSetPattern   -- {head|tail} or {} or {a, b, c}
    , try $ PInt <$> L.decimal
    , try $ PDouble <$> L.float
    , try $ (string "true" >> return (PBool True))
    , try $ (string "false" >> return (PBool False))
    , try $ do _ <- char '\''; c <- L.charLiteral; _ <- char '\''; return (PChar c)
    , try $ do _ <- char '"'; s <- manyTill L.charLiteral (char '"'); return (PString s)
    , do firstChar <- letterChar <|> char '_'
         rest <- many (alphaNumChar <|> char '_')
         return (PVar (firstChar:rest))
    ]

parseADTConstructorPattern :: Parser Pattern
parseADTConstructorPattern = do
    ctorName <- lexeme $ do
        first <- upperChar
        rest <- many (alphaNumChar <|> char '_')
        return (first:rest)
    args <- optional $ between (symbolNoNl "(") (symbolNoNl ")") (parsePatterns)
    case args of
        Just ps -> return $ PADT ctorName ps
        Nothing -> return $ PADT ctorName []

-- Parse list patterns: [], [head|tail], [a, b, c]
parseListPattern :: Parser Pattern
parseListPattern = do
    _ <- char '['
    sc
    result <- choice
        [ try $ do  -- Empty list
            _ <- char ']'
            return (PList [])
        , try $ do  -- Cons pattern [head|tail]
            headPat <- parsePattern
            sc
            _ <- char '|'
            sc
            tailPat <- parsePattern
            sc
            _ <- char ']'
            return (PCons headPat tailPat)
        , do  -- List of patterns [a, b, c]
            pats <- parsePattern `sepBy` symbol ","
            _ <- char ']'
            return (PList pats)
        ]
    return result

-- Parse set patterns: {}, {head|tail}, {a, b, c}
parseSetPattern :: Parser Pattern
parseSetPattern = do
    _ <- char '{'
    sc
    result <- choice
        [ try $ do  -- Empty set
            _ <- char '}'
            return (PSet [])
        , try $ do  -- Set cons pattern {head|tail}
            headPat <- parsePattern
            sc
            _ <- char '|'
            sc
            tailPat <- parsePattern
            sc
            _ <- char '}'
            return (PSetCons headPat tailPat)
        , do  -- Set of patterns {a, b, c}
            pats <- parsePattern `sepBy` symbol ","
            _ <- char '}'
            return (PSet pats)
        ]
    return result

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
        -- Allow zero-or-more horizontal whitespace between the variable and '='
        _ <- optional scn
        _ <- symbol "="
        _ <- optional sc  -- allow newline before rhs
        expr <- parseExpression
        return $ AssignIndex varName idxs expr
    parseSimpleAssign = do
        (varName, idxs) <- lexeme parseVarWithIndices
        if not (null idxs) then fail "indexed" else pure ()
        -- Allow zero-or-more horizontal whitespace between the variable and '='
        _ <- optional scn
        _ <- symbol "="
        _ <- optional sc  -- allow newline before rhs
        -- Check if right-hand side is input() call
        inputCall <- optional $ try $ do
            _ <- keyword "input"
            _ <- symbolNoNl "("
            promptExpr <- optional parseExpression
            _ <- symbolNoNl ")"
            return promptExpr
        case inputCall of
            Just promptExpr -> 
                let prompt = case promptExpr of
                               Just p  -> p
                               Nothing -> StringLiteral ""
                in return $ Input varName prompt
            Nothing -> do
                expr <- parseExpression
                return $ Assign varName expr

parsePrint :: Parser Command
parsePrint = do
    _ <- symbol "print"
    expr <- parseExpression
    return $ Print expr

parseFunctionDef :: Parser Command
parseFunctionDef = do
    _ <- symbol "def"
    name <- lexeme $ do
        first <- letterChar <|> char '_'
        rest <- many (alphaNumChar <|> char '_')
        return (first:rest)
    _ <- symbol "("
    params <- parsePatterns
    _ <- symbol ")"
    body <- (symbolNoNl "->" >> (BodyExpr <$> parseExpression))
        <|> (symbol "do" >> (BodyBlock <$> parseBlockCommands) <* symbol "end")
    return $ FunctionDef name (Clause params body)

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

parseWriteFile :: Parser Command
parseWriteFile = do
    _ <- symbol "writefile"
    filenameExpr <- parseExpression
    contentExpr <- parseExpression
    return $ WriteFile filenameExpr contentExpr


parseReadFile :: Parser Expression
parseReadFile = do
    _ <- try (symbol "read_file")
    _ <- symbolNoNl "("
    filenameExpr <- parseExpression
    _ <- symbolNoNl ")"
    return $ ReadFile filenameExpr

parseFmap :: Parser Expression
parseFmap = do
    _ <- try (symbol "fmap")
    _ <- symbolNoNl "("
    collectionExpr <- parseExpression
    _ <- symbolNoNl ","
    functionExpr <- parseExpression
    _ <- symbolNoNl ")"
    return $ Fmap collectionExpr functionExpr



parseAlgebraicDataType :: Parser Command
parseAlgebraicDataType = do
    _ <- symbol "adt"
    typeName <- lexeme $ do
        firstChar <- letterChar <|> char '_'
        rest <- many (alphaNumChar <|> char '_')
        return (firstChar : rest)
    _ <- symbol "="
    constructors <- parseConstructor `sepBy1` symbol "|"
    return $ AlgebraicTypeDef (ADTDef typeName constructors)
  where
    parseConstructor = do
        consName <- lexeme $ do
            firstChar <- letterChar <|> char '_'
            rest <- many (alphaNumChar <|> char '_')
            return (firstChar : rest)
        argTypes <- many parseType
        return $ ADTConstructor consName argTypes

    parseType = lexeme $ choice
        [ string "Int"    >> return TInt
        , string "Double" >> return TDouble
        , string "Bool"   >> return TBool
        , string "Char"   >> return TChar
        , string "String" >> return TString
        , string "List"   >> return TList
        , string "Set"    >> return TSet
        , do
            first <- letterChar <|> char '_'
            rest <- many (alphaNumChar <|> char '_')
            return (TCustom (first:rest))
        ]

parseRepeat :: Parser Command
parseRepeat = do
    _ <- symbol "for"
    countExpr <- parseExpression
    _ <- symbol "do"
    cmd <- parseBlockCommands
    _ <- symbol "end"
    -- If it's a simple number (possibly wrapped with WithPos), use Repeat; otherwise use While (for conditions)
    case stripPos countExpr of
        Number _ -> return $ Repeat countExpr cmd
        _        -> return $ While countExpr cmd

-- Remove WithPos wrapper to inspect underlying expression
stripPos :: Expression -> Expression
stripPos (WithPos _ e) = stripPos e
stripPos e = e

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
parseSingleCommand = try parseCBinding
               <|> try parseAssignment
               <|> try parseImport
               <|> try parseFunctionDef
               <|> try parseReturn
               <|> try parsePrint
               <|> try parseConditional
               <|> try parseForIn
               <|> try parseRepeat
               <|> try parseWhile
               <|> try parseAlgebraicDataType
               <|> (Print <$> parseExpression)

parseImport :: Parser Command
parseImport = do
    _ <- symbol "import"
    name <- lexeme $ some (letterChar <|> char '_' <|> char '/')
    return (Import name)

parseReturn :: Parser Command
parseReturn = do
    _ <- symbol "return"
    expr <- parseExpression
    return (Return expr)

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

-- Top-level parser. Accepts a source name so parse errors report filenames/line numbers.
parseBernWithName :: String -> String -> Either (ParseErrorBundle String Void) Command
parseBernWithName name input = parse (sc *> parseCommand <* sc <* eof) name input

-- Interactive parser defaults to <interactive>
parseBern :: String -> Either (ParseErrorBundle String Void) Command
parseBern = parseBernWithName "<interactive>"

-- File parser uses the provided filename
parseBernFile :: String -> String -> Either (ParseErrorBundle String Void) Command
parseBernFile = parseBernWithName