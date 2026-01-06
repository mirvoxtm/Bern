module Language.Ast where

    {-
    This is the Abstract Syntax Tree for the Neru Language.
    It defines operations, expressions and types.
    -}

    -- Possible Binary Operations.
    data BinaryOperation = Add           -- (+)
                         | Subtract      -- (-)
                         | Multiply      -- (*)
                         | Divide        -- (/)
                         | Equal         -- (==)
                         | Different     -- (!=) or (~=)
                         | GreaterThan   -- (>)
                         | LessThan      -- (<)
                         | GreaterThanEq -- (>=)
                         | LessThanEq    -- (<=)
                         -- TODO: Add Set Theory Operations (union, intersection, difference, etc.)
                         deriving (Show, Eq)

    -- Unary Operations
    data UnaryOperation = Negate   -- Negates Numbers (-)
                        | Not      -- (!) or (~)
                        deriving (Show, Eq)

    data Expression = Number Int
                    | BoolLiteral Bool -- true/false
                    | BinaryOperator BinaryOperation Expression Expression
                    | UnaryOperator UnaryOperation Expression
                    | Variable String
                    deriving (Show, Eq)

    type Length = Int
    data Value = Integer Int
               | Double Double
               | NaN
               | Undefined
               | Boolean Bool
               | Set [Value] Length          -- TODO: A Set ({}) can hold multiple values of any type
               | List [Value] Length         -- TODO: A List ([]) can hold multiple values of a single type
               | Object [(String, Value)]    -- TODO: An Object (#{ }#) holds key-value pairs
               | TextLiteral String Length   -- TODO: A String ([| |]) that preserves spaces and can add variables directly in it (like ```${name}```)
               | Character Char
               | Text String Length          -- TODO: A String with its length
               | AlgebraicDataType String [Value] -- TODO: An Algebraic Data Type (ADT) with its name and values
               deriving (Show, Eq)

    data Command = Skip
                 | Print Expression
                 | Assign String Expression                  -- (=)
                 | Conditional Expression Command Command    -- if-then-else
                 | Repeat Expression Command                 -- for 3 do {}
                 | While Expression Command                  -- for (x != 10) do
                 | ForIn String Expression Command           --  TODO: for (x:xs) do
                 | Concat Command Command
                 deriving (Show, Eq)