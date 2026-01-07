module Language.Eval where
import Control.Monad (foldM)
import System.Exit (exitFailure)
import Language.Ast
import Data.Hashtable.Hashtable
import Language.Helpers

-- Command interpreter returns the updated hashtable after running a command
interpretCommand :: Command -> Hashtable String Value -> IO (Hashtable String Value)
interpretCommand Skip table = return table

interpretCommand (Print expr) table =
    case evaluate expr table of
        Right val -> putStrLn (prettyValue val) >> return table
        Left err  -> die err

interpretCommand (Assign name expr) table =
    case evaluate expr table of
        Right v@(Integer _)   -> return (insertHashtable table name v)
        Right v@(Double _)    -> return (insertHashtable table name v)
        Right v@(Boolean _)   -> return (insertHashtable table name v)
        Right v@(Text _ _)    -> return (insertHashtable table name v)
        Right v@(Character _) -> return (insertHashtable table name v)
        Right v@(List _ _)    -> return (insertHashtable table name v)
        Right v@(Set _ _)     -> return (insertHashtable table name v)
        Right _               -> die "Unexpected value in assignment"
        Left err              -> die err

interpretCommand (AssignIndex name idxExprs expr) table = do
    baseVal <- case lookupHashtable table name of
                  Just v  -> return v
                  Nothing -> die "Variable not found"
    idxVals <- mapM (\ie -> case evaluate ie table of
                              Right (Integer n) -> return n
                              Right _           -> die "Index must be integer"
                              Left err          -> die err) idxExprs
    newVal <- case evaluate expr table of
                Right v  -> return v
                Left err -> die err
    case setAt idxVals newVal baseVal of
        Right updated -> return (insertHashtable table name updated)
        Left err      -> die err

interpretCommand (Conditional cond thenCmd elseCmd) table =
    case evaluate cond table of
        Right (Boolean True)  -> interpretCommand thenCmd table
        Right (Boolean False) -> interpretCommand elseCmd table
        Right _               -> die "Condition must be boolean"
        Left err              -> die err

interpretCommand (Repeat count cmd) table =
    case evaluate count table of
        Right (Integer n) -> foldM (\t _ -> interpretCommand cmd t) table [1..n]
        Right _           -> die "Repeat count must be integer"
        Left err          -> die err

interpretCommand (While cond cmd) table =
    let loop t =
            case evaluate cond t of
                Right (Boolean True)  -> interpretCommand cmd t >>= loop
                Right (Boolean False) -> return t
                Right _               -> die "While condition must be boolean"
                Left err              -> die err
    in loop table

interpretCommand (ForIn varName collection cmd) table =
    case evaluate collection table of
        Right coll ->
            case toIterable coll of
                Right vals -> foldM (\tbl val -> interpretCommand cmd (insertHashtable tbl varName val)) table vals
                Left err   -> die err
        Left err -> die err

interpretCommand (ForInCount varName idxName collection cmd) table =
    case evaluate collection table of
        Right coll ->
            case toIterable coll of
                Right vals -> foldM (\tbl (val, idx) -> interpretCommand cmd (insertHashtable (insertHashtable tbl varName val) idxName (Integer idx))) table (zip vals [0..])
                Left err   -> die err
        Left err -> die err

interpretCommand (Concat cmd1 cmd2) table = do
    table' <- interpretCommand cmd1 table
    interpretCommand cmd2 table'

-- Fatal error helper
die :: String -> IO a
die msg = putStrLn ("Error: " ++ msg) >> exitFailure

-- Convert iterable values (list, set, text) to a list of values for for-in loops
toIterable :: Value -> Either String [Value]
toIterable (List vals _) = Right vals
toIterable (Set vals _)  = Right vals
toIterable (Text s _)    = Right (map Character s)
toIterable (TextLiteral s _) = Right (map Character s)
toIterable _             = Left "Cannot iterate over this type"

-- Our 'evaluate' function receives an Expression and a Hashtable (for variables)
evaluate :: Expression -> Hashtable String Value -> Either String Value
-- If a Number is received, then it is itself.
evaluate (Number n) _ = (Right (Integer n))

evaluate (DoubleNum d) _ = (Right (Double d))

-- Boolean literal
evaluate (BoolLiteral b) _ = Right (Boolean b)

-- String literal
evaluate (StringLiteral s) _ = Right (Text s (length s))

-- Character literal
evaluate (CharLiteral c) _ = Right (Character c)

-- List literal - all elements must be the same type
evaluate (ListLiteral exprs) table = do
    vals <- mapM (`evaluate` table) exprs
    if null vals 
        then Right (List [] 0)
        else if allSameType vals
            then Right (List vals (length vals))
            else Left "Type error: List elements must all be the same type"

-- Set literal - elements can be of any type
evaluate (SetLiteral exprs) table = do
    vals <- mapM (`evaluate` table) exprs
    Right (Set vals (length vals))

-- Range literal [start .. end]
evaluate (Range start end) table = do
    s <- evaluate start table
    e <- evaluate end table
    case (s, e) of
        (Integer s', Integer e') ->
            let step = if s' <= e' then 1 else -1
                vals = [Integer i | i <- [s', s'+step .. e']]
            in Right (List vals (length vals))
        (Double s', Double e') ->
            let step = if s' <= e' then 1 else -1
                vals = [Double i | i <- [s', s'+step .. e']]
            in Right (List vals (length vals))
        (Integer s', Double e') ->
            let s'' = fromIntegral s'
                step = if s'' <= e' then 1 else -1
                vals = [Double i | i <- [s'', s''+step .. e']]
            in Right (List vals (length vals))
        (Double s', Integer e') ->
            let e'' = fromIntegral e'
                step = if s' <= e'' then 1 else -1
                vals = [Double i | i <- [s', s'+step .. e'']]
            in Right (List vals (length vals))
        _ -> Left "Type error: range bounds must be numeric"

-- Index access: list[i], string[i], set[i]
evaluate (Index expr idxExpr) table = do
    collection <- evaluate expr table
    idx <- evaluate idxExpr table
    case (collection, idx) of
        (List vals len, Integer i) 
            | i >= 0 && i < len -> Right (vals !! i)
            | otherwise -> Left ("Index out of bounds: " ++ show i)
        (Set vals len, Integer i)
            | i >= 0 && i < len -> Right (vals !! i)
            | otherwise -> Left ("Index out of bounds: " ++ show i)
        (Text s len, Integer i)
            | i >= 0 && i < len -> Right (Character (s !! i))
            | otherwise -> Left ("Index out of bounds: " ++ show i)
        _ -> Left "Cannot index this type or invalid index"

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
    if isArithmetic op
        then evalArith op leftVal rightVal
        else if isUnion op
            then evalUnion op leftVal rightVal
            else evalComparison op leftVal rightVal

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
isArithmetic Modulo = True
isArithmetic _ = False

isUnion :: BinaryOperation -> Bool
isUnion Concatenation = True
isUnion Union = True
isUnion Intersection = True
isUnion Difference = True
isUnion _ = False

-- This function evaluates Binary Operations with a Left and Right value
evalArith :: BinaryOperation -> Value -> Value -> Either String Value
-- (x + y)
evalArith Add (Integer l) (Integer r) = Right (Integer (l + r))
evalArith Add (Double l) (Double r) = Right (Double (l + r))
evalArith Add (Integer l) (Double r) = Right (Double (fromIntegral l + r))
evalArith Add (Double l) (Integer r) = Right (Double (l + fromIntegral r))
-- Map scalar addition over list elements
evalArith Add (List vals len) scalar@(Integer _) = do
    vals' <- mapM (numericAdd scalar) vals
    Right (List vals' len)
evalArith Add (List vals len) scalar@(Double _) = do
    vals' <- mapM (numericAdd scalar) vals
    Right (List vals' len)
evalArith Add scalar@(Integer _) (List vals len) = do
    vals' <- mapM (numericAdd scalar) vals
    Right (List vals' len)
evalArith Add scalar@(Double _) (List vals len) = do
    vals' <- mapM (numericAdd scalar) vals
    Right (List vals' len)
evalArith Add (Text l ll) (Text r rl) = Right (Text (l ++ r) (ll + rl))
evalArith Add (Text l ll) (Character r) = Right (Text (l ++ [r]) (ll + 1))
evalArith Add (Character l) (Text r rl) = Right (Text ([l] ++ r) (1 + rl))
evalArith Add (Character l) (Character r) = Right (Text ([l, r]) 2)
evalArith Add (Text l ll) (Integer r) = Right (Text (l ++ show r) (ll + length (show r)))
evalArith Add (Integer l) (Text r rl) = Right (Text (show l ++ r) (length (show l) + rl))
evalArith Add (Text l ll) (Double r) = Right (Text (l ++ show r) (ll + length (show r)))
evalArith Add (Double l) (Text r rl) = Right (Text (show l ++ r) (length (show l) + rl))

-- On the case of lists, if the lists are of the same size, then apply the sum
-- of each element (strictly of same type)
evalArith Add (List l1 len1) (List l2 len2) =
    if len1 /= len2
        then Left "Type error: list arithmetic requires lists of the same length"
        else let combined = zipWith numericAdd l1 l2
             in if all isRight combined
                   then Right (List (map fromRight combined) len1)
                   else Left "Type error: list arithmetic requires numeric elements"
  where
    isRight (Right _) = True
    isRight _        = False
    fromRight (Right v) = v
    fromRight _         = error "Unexpected Left value"

evalArith Add (Set s1 len1) v =
    if v `elem` s1
        then Right (Set s1 len1)
        else Right (Set (s1 ++ [v]) (len1 + 1))


-- (x - y)
evalArith Subtract (Integer l) (Integer r) = Right (Integer (l - r))
evalArith Subtract (Double l) (Double r) = Right (Double (l - r))
evalArith Subtract (Integer l) (Double r) = Right (Double (fromIntegral l - r))
evalArith Subtract (Double l) (Integer r) = Right (Double (l - fromIntegral r))
-- Map scalar subtraction over list elements (element - scalar)
evalArith Subtract (List vals len) scalar@(Integer _) = do
    vals' <- mapM (\v -> numericSub v scalar) vals
    Right (List vals' len)
evalArith Subtract (List vals len) scalar@(Double _) = do
    vals' <- mapM (\v -> numericSub v scalar) vals
    Right (List vals' len)
-- scalar - list (scalar minus each element)
evalArith Subtract scalar@(Integer _) (List vals len) = do
    vals' <- mapM (numericSub scalar) vals
    Right (List vals' len)
evalArith Subtract scalar@(Double _) (List vals len) = do
    vals' <- mapM (numericSub scalar) vals
    Right (List vals' len)

evalArith Subtract (List l1 len1) (List l2 len2) =
    if len1 /= len2
        then Left "Type error: list arithmetic requires lists of the same length"
        else let combined = zipWith numericSub l1 l2
             in if all isRight combined
                   then Right (List (map fromRight combined) len1)
                   else Left "Type error: list arithmetic requires numeric elements"
  where
    isRight (Right _) = True
    isRight _        = False
    fromRight (Right v) = v
    fromRight _         = error "Unexpected Left value"

evalArith Subtract (Set s1 len1) v =
    if v `elem` s1
        then Right (Set (filter (/= v) s1) (len1 - 1))
        else Right (Set s1 len1)

-- (x * y)
evalArith Multiply (Integer l) (Integer r) = Right (Integer (l * r))
evalArith Multiply (Double l) (Double r) = Right (Double (l * r))
evalArith Multiply (Integer l) (Double r) = Right (Double (fromIntegral l * r))
evalArith Multiply (Double l) (Integer r) = Right (Double (l * fromIntegral r))
-- Map scalar multiplication over list elements
evalArith Multiply (List vals len) scalar@(Integer _) = do
    vals' <- mapM (numericMul scalar) vals
    Right (List vals' len)
evalArith Multiply (List vals len) scalar@(Double _) = do
    vals' <- mapM (numericMul scalar) vals
    Right (List vals' len)
evalArith Multiply scalar@(Integer _) (List vals len) = do
    vals' <- mapM (numericMul scalar) vals
    Right (List vals' len)
evalArith Multiply scalar@(Double _) (List vals len) = do
    vals' <- mapM (numericMul scalar) vals
    Right (List vals' len)

evalArith Multiply (List l1 len1) (List l2 len2) =
    if len1 /= len2
        then Left "Type error: list arithmetic requires lists of the same length"
        else let combined = zipWith numericMul l1 l2
             in if all isRight combined
                   then Right (List (map fromRight combined) len1)
                   else Left "Type error: list arithmetic requires numeric elements"
  where
    isRight (Right _) = True
    isRight _        = False
    fromRight (Right v) = v
    fromRight _         = error "Unexpected Left value"

-- (x / y)
evalArith Divide (Integer l) (Integer r)
    -- Not A Number when attempting to divide by Zero
    | r == 0    = Right NaN
    | otherwise = Right (Integer (l `div` r))

evalArith Divide (Double l) (Double r)
    -- Not A Number when attempting to divide by Zero
    | r == 0.0  = Right NaN
    | otherwise = Right (Double (l / r))

evalArith Divide (Integer l) (Double r)
    -- Not A Number when attempting to divide by Zero
    | r == 0.0  = Right NaN
    | otherwise = Right (Double (fromIntegral l / r))

evalArith Divide (Double l) (Integer r)
    -- Not A Number when attempting to divide by Zero
    | r == 0    = Right NaN
    | otherwise = Right (Double (l / fromIntegral r))
-- (x % y)
evalArith Modulo (Integer l) (Integer r)
    | r == 0    = Right NaN
    | otherwise = Right (Integer (l `mod` r))
evalArith Modulo _ _ = Left "Type error: modulo requires integers"

-- Map scalar division over list elements
evalArith Divide (List vals len) scalar@(Integer r)
    | r == 0 = Right (List (replicate len NaN) len)
    | otherwise = do
        vals' <- mapM (numericDiv scalar) vals
        Right (List vals' len)
evalArith Divide (List vals len) scalar@(Double r)
    | r == 0 = Right (List (replicate len NaN) len)
    | otherwise = do
        vals' <- mapM (numericDiv scalar) vals
        Right (List vals' len)
evalArith Divide scalar@(Integer _) (List vals len) = do
    vals' <- mapM (numericDivLeft scalar) vals
    Right (List vals' len)
evalArith Divide scalar@(Double _) (List vals len) = do
    vals' <- mapM (numericDivLeft scalar) vals
    Right (List vals' len)

evalArith Divide (List l1 len1) (List l2 len2) =
    if len1 /= len2
        then Left "Type error: list arithmetic requires lists of the same length"
        else let combined = zipWith numericDiv l1 l2
             in if all isRight combined
                   then Right (List (map fromRight combined) len1)
                   else Left "Type error: list arithmetic requires numeric elements"
  where
    isRight (Right _) = True
    isRight _        = False
    fromRight (Right v) = v
    fromRight _         = error "Unexpected Left value"

evalArith Modulo (Integer l) (Integer r)
    | r == 0    = Right NaN
    | otherwise = Right (Integer (l `mod` r))

evalArith Modulo (Double l) (Double r)
    | r == 0.0  = Right NaN
    | otherwise = Right (Double (mod' l r))
  where
    mod' x y = x - y * fromIntegral (floor (x / y))

evalArith Modulo (Integer l) (Double r)
    | r == 0.0  = Right NaN
    | otherwise = Right (Double (mod' (fromIntegral l) r))
  where
    mod' x y = x - y * fromIntegral (floor (x / y))

evalArith Modulo (Double l) (Integer r)
    | r == 0    = Right NaN
    | otherwise = Right (Double (mod' l (fromIntegral r)))
  where
    mod' x y = x - y * fromIntegral (floor (x / y))

-- Unsupported types for arithmetic operations
evalArith _ _ _ = Left "Type error in arithmetic operation"

-- Numeric helpers
numericAdd :: Value -> Value -> Either String Value
numericAdd (Integer l) (Integer r) = Right (Integer (l + r))
numericAdd (Double l) (Double r) = Right (Double (l + r))
numericAdd (Integer l) (Double r) = Right (Double (fromIntegral l + r))
numericAdd (Double l) (Integer r) = Right (Double (l + fromIntegral r))
numericAdd _ _ = Left "Type error: list arithmetic requires numeric elements"

numericSub :: Value -> Value -> Either String Value
numericSub (Integer l) (Integer r) = Right (Integer (l - r))
numericSub (Double l) (Double r) = Right (Double (l - r))
numericSub (Integer l) (Double r) = Right (Double (fromIntegral l - r))
numericSub (Double l) (Integer r) = Right (Double (l - fromIntegral r))
numericSub _ _ = Left "Type error: list arithmetic requires numeric elements"

numericMul :: Value -> Value -> Either String Value
numericMul (Integer l) (Integer r) = Right (Integer (l * r))
numericMul (Double l) (Double r) = Right (Double (l * r))
numericMul (Integer l) (Double r) = Right (Double (fromIntegral l * r))
numericMul (Double l) (Integer r) = Right (Double (l * fromIntegral r))
numericMul _ _ = Left "Type error: list arithmetic requires numeric elements"

numericDiv :: Value -> Value -> Either String Value
numericDiv (Integer l) (Integer r)
    | r == 0    = Right NaN
    | otherwise = Right (Integer (l `div` r))
numericDiv (Double l) (Double r)
    | r == 0.0  = Right NaN
    | otherwise = Right (Double (l / r))
numericDiv (Integer l) (Double r)
    | r == 0.0  = Right NaN
    | otherwise = Right (Double (fromIntegral l / r))
numericDiv (Double l) (Integer r)
    | r == 0    = Right NaN
    | otherwise = Right (Double (l / fromIntegral r))
numericDiv _ _ = Left "Type error: list arithmetic requires numeric elements"

-- scalar / element
numericDivLeft :: Value -> Value -> Either String Value
numericDivLeft = flip numericDiv

-- Immutable update of nested list elements
setAt :: [Int] -> Value -> Value -> Either String Value
setAt [] _ _ = Left "Invalid assignment target"
setAt (i:is) newVal (List vals len)
    | i < 0 || i >= len = Left "Index out of bounds"
    | null is =
        -- replace element, enforce homogeneous list
        let old = vals !! i
        in if getValueType old == getValueType newVal
              then Right (List (replaceAt i newVal vals) len)
              else Left "Type error: assigned value type differs from list element type"
    | otherwise = do
        nested <- case vals !! i of
                    l@(List _ _) -> Right l
                    _            -> Left "Type error: cannot index into non-list"
        updatedNested <- setAt is newVal nested
        Right (List (replaceAt i updatedNested vals) len)
setAt _ _ _ = Left "Type error: assignment target must be a list"

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 newVal (_:xs) = newVal : xs
replaceAt n newVal (x:xs) = x : replaceAt (n-1) newVal xs

evalComparison :: BinaryOperation -> Value -> Value -> Either String Value
-- (x == y)
evalComparison Equal (Integer l) (Integer r) = Right (Boolean (l == r))
evalComparison Equal (Double l) (Double r) = Right (Boolean (l == r))
evalComparison Equal (Integer l) (Double r) = Right (Boolean (fromIntegral l == r))
evalComparison Equal (Double l) (Integer r) = Right (Boolean (l == fromIntegral r))
evalComparison Equal (Boolean l) (Boolean r) = Right (Boolean (l == r))
evalComparison Equal (Text l _) (Text r _) = Right (Boolean (l == r))
evalComparison Equal (Character l) (Character r) = Right (Boolean (l == r))
evalComparison Equal (List l _) (List r _) = Right (Boolean (l == r))
evalComparison Equal (Set l _) (Set r _) = Right (Boolean (l == r))
-- (x != y)
evalComparison Different (Integer l) (Integer r) = Right (Boolean (l /= r))
evalComparison Different (Double l) (Double r) = Right (Boolean (l /= r))
evalComparison Different (Boolean l) (Boolean r) = Right (Boolean (l /= r))
evalComparison Different (Text l _) (Text r _) = Right (Boolean (l /= r))
evalComparison Different (Character l) (Character r) = Right (Boolean (l /= r))
evalComparison Different (List l _) (List r _) = Right (Boolean (l /= r))
evalComparison Different (Set l _) (Set r _) = Right (Boolean (l /= r))

-- (x && y)
evalComparison And (Boolean l) (Boolean r) = Right (Boolean (l && r))
evalComparison And _ _ = Left "Type error in logical operation"

-- (x || y)
evalComparison Or (Boolean l) (Boolean r) = Right (Boolean (l || r))
evalComparison Or _ _ = Left "Type error in logical operation"

-- (x > y)
evalComparison GreaterThan (Integer l) (Integer r) = Right (Boolean (l > r))
evalComparison GreaterThan (Double l) (Double r) = Right (Boolean (l > r))
evalComparison GreaterThan (Text l ls) (Text r rs) = Right (Boolean (ls > rs))
evalComparison GreaterThan (List l ls) (List r rs) = Right (Boolean (ls > rs))
evalComparison GreaterThan (Set l ls) (Set r rs) = Right (Boolean (ls > rs))

-- (x < y)
evalComparison LessThan (Integer l) (Integer r) = Right (Boolean (l < r))
evalComparison LessThan (Double l) (Double r) = Right (Boolean (l < r))
evalComparison LessThan (Text l ls) (Text r rs) = Right (Boolean (ls < rs))
evalComparison LessThan (List l ls) (List r rs) = Right (Boolean (ls < rs))
evalComparison LessThan (Set l ls) (Set r rs) = Right (Boolean (ls < rs))
-- (x >= y)
evalComparison GreaterThanEq (Integer l) (Integer r) = Right (Boolean (l >= r))
evalComparison GreaterThanEq (Double l) (Double r) = Right (Boolean (l >= r))
evalComparison GreaterThanEq (Text l ls) (Text r rs) = Right (Boolean (ls >= rs))
evalComparison GreaterThanEq (List l ls) (List r rs) = Right (Boolean (ls >= rs))
evalComparison GreaterThanEq (Set l ls) (Set r rs) = Right (Boolean (ls >= rs))
-- (x <= y)
evalComparison LessThanEq (Integer l) (Integer r) = Right (Boolean (l <= r))
evalComparison LessThanEq (Double l) (Double r) = Right (Boolean (l <= r))
evalComparison LessThanEq (Text l ls) (Text r rs) = Right (Boolean (ls <= rs))
evalComparison LessThanEq (List l ls) (List r rs) = Right (Boolean (ls <= rs))
evalComparison LessThanEq (Set l ls) (Set r rs) = Right (Boolean (ls <= rs))

-- In case of an error
evalComparison _ _ _ = Left "Type error in binary operation"

-- Evaluate unary operations
evalUnaryOp :: UnaryOperation -> Value -> Either String Value
evalUnaryOp Negate (Integer n) = Right (Integer (-n))
evalUnaryOp Negate (Double d) = Right (Double (-d))
evalUnaryOp Not (Boolean b) = Right (Boolean (not b))
evalUnaryOp TypeOf v = Right (Text (getValueType v) (length (getValueType v)))
evalUnaryOp SizeOf (Text _ len) = Right (Integer len)
evalUnaryOp SizeOf (List _ len) = Right (Integer len)
evalUnaryOp SizeOf (Set _ len) = Right (Integer len)
evalUnaryOp _ _ = Left "Type error in unary operation"

evalUnion :: BinaryOperation -> Value -> Value -> Either String Value
-- (a <> b)
evalUnion Concatenation (List l1 len1) (List l2 len2) =
    let combined = l1 ++ l2
    in if allSameType combined
          then Right (List combined (len1 + len2))
          else Left "Type error: list concatenation requires elements of the same type"
evalUnion Concatenation (Set s1 len1) (Set s2 len2) =
    let combined = s1 ++ filter (`notElem` s1) s2
    in Right (Set combined (length combined))

evalUnion Concatenation (Text t1 len1) (Text t2 len2) = Right (Text (t1 ++ t2) (len1 + len2))

-- (a <| b) - Union
evalUnion Union (Set s1 len1) (Set s2 len2) =
    let combined = s1 ++ filter (`notElem` s1) s2
    in Right (Set combined (length combined))
evalUnion Union (List l1 len1) (List l2 len2) =
    let combined = l1 ++ filter (`notElem` l1) l2
    in Right (List combined (length combined))
evalUnion Union (Text t1 len1) (Text t2 len2) =
    let combinedStr = t1 ++ filter (`notElem` t1) t2
    in Right (Text combinedStr (length combinedStr))

-- (a |> b) - Intersection
evalUnion Intersection (Set s1 len1) (Set s2 len2) =
    let common = filter (`elem` s2) s1
    in Right (Set common (length common))
evalUnion Intersection (List l1 len1) (List l2 len2) =
    let common = filter (`elem` l2) l1
    in Right (List common (length common))
evalUnion Intersection (Text t1 len1) (Text t2 len2) =
    let commonChars = filter (`elem` t2) t1
    in Right (Text commonChars (length commonChars))

-- (a </> b) - Difference
evalUnion Difference (Set s1 len1) (Set s2 len2) =
    let diff = filter (`notElem` s2) s1
    in Right (Set diff (length diff))
evalUnion Difference (List l1 len1) (List l2 len2) =
    let diff = filter (`notElem` l2) l1
    in Right (List diff (length diff))
evalUnion Difference (Text t1 len1) (Text t2 len2) =
    let diffChars = filter (`notElem` t2) t1
    in Right (Text diffChars (length diffChars))

evalUnion _ _ _ = Left "Type error in Array operation"

-- Pretty print a value (without type constructor names)
prettyValue :: Value -> String
prettyValue (Integer n) = show n
prettyValue (Double d) = show d
prettyValue NaN = "NaN"
prettyValue Undefined = "undefined"
prettyValue (Boolean True) = "true"
prettyValue (Boolean False) = "false"
prettyValue (Text s _) = s
prettyValue (TextLiteral s _) = s
prettyValue (Character c) = [c]
prettyValue (List vals _) = "[" ++ intercalateWith ", " (map prettyValue vals) ++ "]"
prettyValue (Set vals _) = "{" ++ intercalateWith ", " (map prettyValue vals) ++ "}"
prettyValue (Object pairs) = "#{" ++ intercalateWith ", " (map (\(k,v) -> k ++ ": " ++ prettyValue v) pairs) ++ "}#"
prettyValue (AlgebraicDataType name vals) = name ++ "(" ++ intercalateWith ", " (map prettyValue vals) ++ ")"

-- Helper for joining strings
intercalateWith :: String -> [String] -> String
intercalateWith _ [] = ""
intercalateWith _ [x] = x
intercalateWith sep (x:xs) = x ++ sep ++ intercalateWith sep xs

