module Language.Ast where

import Text.Megaparsec (SourcePos)

{-
This is the Abstract Syntax Tree for the Bern Language.
It defines operations, expressions and types.
-}

-- Possible Binary Operations.
data BinaryOperation = Add           -- (+)
                     | Subtract      -- (-)
                     | Multiply      -- (*)
                     | Divide        -- (/)
                     | Modulo        -- (%)
                     | Equal         -- (==)
                     | Different     -- (!=) or (~=)
                     | And           -- (&&)
                     | Or            -- (||)
                     | GreaterThan   -- (>)
                     | LessThan      -- (<)
                     | GreaterThanEq -- (>=)
                     | LessThanEq    -- (<=)
                     | Concatenation        -- (<>)
                     | Union               -- (<|) for Sets and Lists
                     | Intersection        -- (|>) for Sets and Lists
                     | Difference          -- (</>) for Sets and Lists
                     deriving (Show, Eq)

-- Unary Operations
data UnaryOperation = Negate   -- Negates Numbers (-)
                    | Not      -- (!) or (~)
                    | TypeOf   -- :: expr
                    | SizeOf   -- :> expr
                    deriving (Show, Eq)

data AlgebraicDataTypeDef = ADTDef String [ADTConstructor]
    deriving (Show, Eq)

data ADTConstructor = ADTConstructor String [Type]
    deriving (Show, Eq)

data Type = TInt | TDouble | TBool | TChar | TString | TList | TSet | TCustom String
    deriving (Show, Eq)

data Expression = Number Int
                | AlgebraicDataTypeConstruct String [Expression] -- ADT Name and its values
                | DoubleNum Double
                | BoolLiteral Bool -- true/false
                | StringLiteral String -- "string"
                | CharLiteral Char -- 'c'
                | SetLiteral [Expression]      -- {elem1, elem2, ...}
                | ListLiteral [Expression]     -- [elem1, elem2, ...]
                | ObjectLiteral [(String, Expression)] -- #{ key: value, ... }#
                | Range Expression Expression          -- [start .. end]
                | Index Expression Expression  -- list[index], set[index] or string[index]
                | BinaryOperator BinaryOperation Expression Expression
                | UnaryOperator UnaryOperation Expression
                | Variable String
                | FunctionCall String [Expression]
                | LambdaExpr [Pattern] Expression
                | WithPos SourcePos Expression        -- carries source position for better errors
                | ReadFile Expression                 -- readfile filename
                | GetHostMachine                      -- get_host_machine()
                | Fmap Expression Expression          -- fmap collection function
                deriving (Show, Eq)

-- Patterns for function parameters (used by defs and lambdas)
data Pattern = PVar String
            | PInt Int
            | PADT String [Pattern]      -- ADT pattern matching
            | PDouble Double
            | PBool Bool
            | PChar Char
            | PString String
            | PWildcard
            | PList [Pattern]           -- Empty list [] or list of patterns [a, b, c]
            | PCons Pattern Pattern     -- Cons pattern [head|tail]
            | PSet [Pattern]            -- Empty set {} or set of patterns {a, b, c}
            | PSetCons Pattern Pattern  -- Set cons pattern {head|tail}
            deriving (Show, Eq)

data FunctionBody = BodyExpr Expression
                    | BodyBlock Command
                    deriving (Show, Eq)

type Length = Int
data Value = Integer Int
            | Double Double
            | NaN
            | Undefined
            | Boolean Bool
            | Set [Value] Length          -- A Set ({}) can hold multiple values of any type
            | List [Value] Length         -- A List ([]) can hold multiple values of a single type
            | Object [(String, Value)]    -- An Object (#{ }#) holds key-value pairs
            | TextLiteral String Length   -- A String ([| |]) that preserves spaces and can add variables directly in it (like ```${name}```)
            | Character Char
            | Function [Clause]
            | Lambda [Clause]
            | CBinding String ([Value] -> IO Value)  -- A C function binding (runtime value)
            | AlgebraicDataType String [Value] -- An Algebraic Data Type (ADT) with its name and values

-- IMPORTANTE: Não podemos derivar Show e Eq automaticamente porque
-- ([Value] -> IO Value) não tem instâncias de Show e Eq
-- Você precisará implementar manualmente:

instance Show Value where
    show (Integer n) = "Integer " ++ show n
    show (Double d) = "Double " ++ show d
    show NaN = "NaN"
    show Undefined = "Undefined"
    show (Boolean b) = "Boolean " ++ show b
    show (Set vs l) = "Set " ++ show vs ++ " " ++ show l
    show (List vs l) = "List " ++ show vs ++ " " ++ show l
    show (Object kvs) = "Object " ++ show kvs
    show (TextLiteral s l) = "TextLiteral " ++ show s ++ " " ++ show l
    show (Character c) = "Character " ++ show c
    show (Function cs) = "Function " ++ show cs
    show (Lambda cs) = "Lambda " ++ show cs
    show (CBinding name _) = "CBinding " ++ show name ++ " <function>"  -- Não podemos mostrar a função
    show (AlgebraicDataType name vs) = "AlgebraicDataType " ++ show name ++ " " ++ show vs

instance Eq Value where
    (Integer a) == (Integer b) = a == b
    (Double a) == (Double b) = a == b
    NaN == NaN = True
    Undefined == Undefined = True
    (Boolean a) == (Boolean b) = a == b
    (Set a la) == (Set b lb) = a == b && la == lb
    (List a la) == (List b lb) = a == b && la == lb
    (Object a) == (Object b) = a == b
    (TextLiteral a la) == (TextLiteral b lb) = a == b && la == lb
    (Character a) == (Character b) = a == b
    (Function a) == (Function b) = a == b
    (Lambda a) == (Lambda b) = a == b
    (CBinding na _) == (CBinding nb _) = na == nb  -- Comparamos apenas pelo nome
    (AlgebraicDataType na va) == (AlgebraicDataType nb vb) = na == nb && va == vb
    _ == _ = False

-- One function can have multiple pattern-matching clauses.
data Clause = Clause [Pattern] FunctionBody
    deriving (Show, Eq)

data Command = Skip
                | Print Expression
                | Assign String Expression                    -- (=)
                | AssignIndex String [Expression] Expression  -- var[idx..] = expr
                | Conditional Expression Command Command      -- if-then-else
                | Repeat Expression Command                   -- for 3 do {}
                | While Expression Command                    -- for (x != 10) do
                | ForIn String Expression Command             -- for x : collection do ... end
                | ForInCount String String Expression Command -- for x,i : collection do ... end
                | Concat Command Command
                | FunctionDef String Clause                   -- def name(params) do ... end / -> expr / pattern clauses
                | Return Expression                           -- return expr
                | Import String                               -- import module
                | Input String Expression                     -- input var prompt
                | WriteFile Expression Expression             -- writefile filename expr
                | AlgebraicTypeDef AlgebraicDataTypeDef
                | CForeignDecl String Expression [String] String -- foreign funcName (argTypes) -> retType
                deriving (Show, Eq)