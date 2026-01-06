module Language.Eval where
import Control.Monad (foldM)
import Language.Ast
import Data.Hashtable.Hashtable

-- Command interpreter returns the updated hashtable after running a command
interpretCommand :: Command -> Hashtable String Value -> IO (Hashtable String Value)
interpretCommand Skip table = return table

interpretCommand (Print expr) table =
    case evaluate expr table of
        Right val -> print val >> return table
        Left err  -> putStrLn ("Error: " ++ err) >> return table

interpretCommand (Assign name expr) table =
    case evaluate expr table of
        Right v@(Integer _) -> return (insertHashtable table name v)
        Right v@(Boolean _) -> return (insertHashtable table name v)
        Right _             -> return table
        Left err            -> putStrLn ("Error: " ++ err) >> return table

interpretCommand (Conditional cond thenCmd elseCmd) table =
    case evaluate cond table of
        Right (Boolean True)  -> interpretCommand thenCmd table
        Right (Boolean False) -> interpretCommand elseCmd table
        _                     -> return table

interpretCommand (Repeat count cmd) table =
    case evaluate count table of
        Right (Integer n) -> foldM (\t _ -> interpretCommand cmd t) table [1..n]
        _                 -> return table

interpretCommand (While cond cmd) table =
    let loop t =
            case evaluate cond t of
                Right (Boolean True)  -> interpretCommand cmd t >>= loop
                Right (Boolean False) -> return t
                _                     -> return t
    in loop table

interpretCommand (Concat cmd1 cmd2) table = do
    table' <- interpretCommand cmd1 table
    interpretCommand cmd2 table'

-- Our 'evaluate' function receives an Expression and a Hashtable (for variables)
evaluate :: Expression -> Hashtable String Value -> Either String Value
-- If a Number is received, then it is itself.
evaluate (Number n) _ = Right (Integer n)

-- Boolean literal
evaluate (BoolLiteral b) _ = Right (Boolean b)

-- If a Variable name is received, then
evaluate (Variable name) table =
    -- Check if the name of the variable is on the Hashtable
    case lookupHashtable table name of
        -- If it is, then it is itself
        Just val -> Right val
        -- Otherwise, this variable was not defined.
        Nothing  -> Right Undefined

-- If a Binary Operation is received, then
evaluate (BinaryOperator op left right) table = do
    -- Evaluate Left and Right until atom value
    leftVal <- evaluate left table
    rightVal <- evaluate right table

    if isArithmetic op then evalArith op leftVal rightVal else evalComparison op leftVal rightVal
        
-- If a Unary Operation is received, then
evaluate (UnaryOperator op expr) table = do
    -- Evaluate the expression until atom
    val <- evaluate expr table
    -- Call evalUnaryOp when value is fully found.
    evalUnaryOp op val

evaluate _ _ = Left "Unsupported Expression"

-- Check if said Binary Operation is Arithmetic
isArithmetic :: BinaryOperation -> Bool
isArithmetic Add = True
isArithmetic Subtract = True
isArithmetic Multiply = True
isArithmetic Divide = True
isArithmetic _ = False

-- This function evaluates Binary Operations with a Left and Right value
evalArith :: BinaryOperation -> Value -> Value -> Either String Value
-- (x + y)
evalArith Add (Integer l) (Integer r) = Right (Integer (l + r))

-- (x - y)
evalArith Subtract (Integer l) (Integer r) = Right (Integer (l - r))

-- (x * y)
evalArith Multiply (Integer l) (Integer r) = Right (Integer (l * r))

-- (x / y)
evalArith Divide (Integer l) (Integer r)
    -- Not A Number when attempting to divide by Zero
    | r == 0    = Right NaN
    | otherwise = Right (Integer (l `div` r))

-- Unsupported types for arithmetic operations
evalArith _ _ _ = Left "Type error in arithmetic operation"

evalComparison :: BinaryOperation -> Value -> Value -> Either String Value
-- (x == y)
evalComparison Equal (Integer l) (Integer r) = Right (Boolean (l == r))
evalComparison Equal (Boolean l) (Boolean r) = Right (Boolean (l == r))
-- (x != y)
evalComparison Different (Integer l) (Integer r) = Right (Boolean (l /= r))
evalComparison Different (Boolean l) (Boolean r) = Right (Boolean (l /= r))
-- (x > y)
evalComparison GreaterThan (Integer l) (Integer r) = Right (Boolean (l > r))
-- (x < y)
evalComparison LessThan (Integer l) (Integer r) = Right (Boolean (l < r))
-- (x >= y)
evalComparison GreaterThanEq (Integer l) (Integer r) = Right (Boolean (l >= r))
-- (x <= y)
evalComparison LessThanEq (Integer l) (Integer r) = Right (Boolean (l <= r))

-- In case of an error
evalComparison _ _ _ = Left "Type error in binary operation"

-- Evaluate unary operations
evalUnaryOp :: UnaryOperation -> Value -> Either String Value
evalUnaryOp Negate (Integer n) = Right (Integer (-n))
evalUnaryOp Not (Boolean b) = Right (Boolean (not b))
evalUnaryOp _ _ = Left "Type error in unary operation"

