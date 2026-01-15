module Language.Eval where
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (doesFileExist, getCurrentDirectory)
import Data.Maybe (isJust)
import Debug.Trace (trace)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, (</>))
import Language.Ast
import Data.Hashtable.Hashtable
import Language.Helpers
import System.Info (os)
import Parsing.Parser (parseBernFile)
import Text.Megaparsec (errorBundlePretty, sourcePosPretty, SourcePos)
import qualified Language.FFI as FFI
interpretCommand :: Maybe SourcePos -> Command -> Hashtable String Value -> IO (Hashtable String Value)
interpretCommand mpos Skip table = return table

interpretCommand mpos (Input varName promptExpr) table = do
    promptVal <- case evaluate promptExpr table of
                    Right v -> case valueToString v of
                                  Just s  -> return s
                                  Nothing -> die (posPrefix mpos "Prompt must be a string")
                    Left err -> die (posPrefix mpos err)
    putStr promptVal
    input <- getLine
    let newTable = insertHashtable table varName (List (map Character input) (length input))
    return newTable

interpretCommand mpos (WriteFile filePath contentExpr) table = do
    pathVal <- case evaluate filePath table of
                    Right v -> case valueToString v of
                                  Just s  -> return s
                                  Nothing -> die (posPrefix mpos "File path must be a string")
                    Left err -> die (posPrefix mpos err)
    contentVal <- case evaluate contentExpr table of
                    Right v -> case valueToString v of
                                  Just s  -> return s
                                  Nothing -> die (posPrefix mpos "Content must be a string")
                    Left err -> die (posPrefix mpos err)
    writeFile pathVal contentVal
    return table

interpretCommand mpos (Print expr) table =
    case evaluate expr table of
        Right Undefined -> return table
        Right val -> putStrLn (prettyValue val) >> return table
        Left err  -> die (posPrefix mpos err)

interpretCommand mpos (Assign name expr) table =
    case evaluate expr table of
        Right v@(Integer _)   -> return (insertHashtable table name v)
        Right v@(Double _)    -> return (insertHashtable table name v)
        Right v@(Boolean _)   -> return (insertHashtable table name v)
        Right v@(Character _) -> return (insertHashtable table name v)
        Right v@(List _ _)    -> return (insertHashtable table name v)
        Right v@(Set _ _)     -> return (insertHashtable table name v)
        Right v@(Object _)    -> return (insertHashtable table name v)
        Right v@(Function _)  -> return (insertHashtable table name v)
        Right v@(Lambda _)    -> return (insertHashtable table name v)
        Right v@(AlgebraicDataType _ _) -> return (insertHashtable table name v)
        Right v@(CBinding _ _) -> return (insertHashtable table name v)
        Right v@(NaN)          -> return (insertHashtable table name v)
        Right v               -> die (posPrefix mpos ("Unexpected value in assignment: " ++ show v ++ " at variable " ++ name ++ " in line " ++ maybe "unknown" sourcePosPretty mpos))
        Left err              -> die (posPrefix mpos err)

interpretCommand mpos (AssignIndex name idxExprs expr) table = do
    baseVal <- case lookupHashtable table name of
                  Just v  -> return v
                  Nothing -> die (posPrefix mpos "Variable not found")
    idxVals <- mapM (\ie -> case evaluate ie table of
                              Right (Integer n)      -> return (IdxInt n)
                              Right (Character c)    -> return (IdxKey [c])
                              Right l@(List _ _)     ->
                                  case valueToString l of
                                    Just s  -> return (IdxKey s)
                                    Nothing -> die (posPrefix mpos "Index must be integer or string (in case of objects)")
                              Right _                -> die (posPrefix mpos "Index must be integer or string (in case of objects)")
                              Left err               -> die (posPrefix mpos err)) idxExprs
    newVal <- case evaluate expr table of
                Right v  -> return v
                Left err -> die (posPrefix mpos err)
    case setAt idxVals newVal baseVal of
        Right updated -> return (insertHashtable table name updated)
        Left err      -> die (posPrefix mpos err)

interpretCommand mpos (Conditional cond thenCmd elseCmd) table =
    case evaluate cond table of
        Right (Boolean True)  -> interpretCommand mpos thenCmd table
        Right (Boolean False) -> interpretCommand mpos elseCmd table
        Right _               -> die (posPrefix mpos "Condition must be boolean")
        Left err              -> die (posPrefix mpos err)

interpretCommand mpos (Repeat count cmd) table =
    case evaluate count table of
        Right (Integer n) -> loop n table
        Right _           -> die (posPrefix mpos "Repeat count must be integer")
        Left err          -> die (posPrefix mpos err)
  where
    loop 0 t = return t
    loop k t = do
        t' <- interpretCommand mpos cmd t
        if hasReturn t'
            then return t'
            else loop (k-1) t'

interpretCommand mpos (While cond cmd) table =
    let loop t =
            case evaluate cond t of
                Right (Boolean True)  -> do
                    t' <- interpretCommand mpos cmd t
                    if hasReturn t'
                        then return t'
                        else loop t'
                Right (Boolean False) -> return t
                Right _               -> die (posPrefix mpos "While condition must be boolean")
                Left err              -> die (posPrefix mpos err)
    in loop table

interpretCommand mpos (ForIn varName collection cmd) table =
    case evaluate collection table of
        Right coll ->
            case toIterable coll of
                Right vals -> loop vals table
                Left err   -> die (posPrefix mpos err)
        Left err -> die (posPrefix mpos err)
  where
    loop [] t = return t
    loop (v:vs) t = do
        t' <- interpretCommand mpos cmd (insertHashtable t varName v)
        if hasReturn t'
            then return t'
            else loop vs t'

interpretCommand mpos (ForInCount varName idxName collection cmd) table =
    case evaluate collection table of
        Right coll ->
            case toIterable coll of
                Right vals -> loop (zip vals [0..]) table
                Left err   -> die (posPrefix mpos err)
        Left err -> die (posPrefix mpos err)
  where
    loop [] t = return t
    loop ((v,idx):rest) t = do
        let tWith = insertHashtable (insertHashtable t varName v) idxName (Integer idx)
        t' <- interpretCommand mpos cmd tWith
        if hasReturn t'
            then return t'
            else loop rest t'

interpretCommand mpos (FunctionDef name clause) table = do
    let newFunc = case lookupHashtable table name of
                    Just (Function cs) -> Function (cs ++ [clause])
                    Just _             -> Function [clause]
                    Nothing            -> Function [clause]
    return (insertHashtable table name newFunc)

interpretCommand mpos (Return expr) table =
    case evaluate expr table of
        Right v -> return (insertHashtable table "__return" v)
        Left err -> die (posPrefix mpos err)

interpretCommand mpos (AlgebraicTypeDef (ADTDef typeName constructors)) table = do
    let adtVal = AlgebraicDataType typeName []
    let newTable = insertHashtable table typeName adtVal
    let tableWithCtors = foldl (\tbl (ADTConstructor ctorName _) ->
                                  insertHashtable tbl ctorName (Function [])) newTable constructors
    
    case constructors of
        [ADTConstructor _ fieldTypes] -> do
            let fieldTypeNames = map typeToString fieldTypes
            let layout = createStructLayout typeName fieldTypeNames
            FFI.registerStruct layout
        _ -> return () 
    
    return tableWithCtors
  where
    typeToString :: Type -> String
    typeToString TInt = "Int"
    typeToString TDouble = "Double"
    typeToString TBool = "Bool"
    typeToString TChar = "Char"
    typeToString TString = "String"
    typeToString TList = "List"
    typeToString TSet = "Set"
    typeToString (TCustom name) = name
    
    createStructLayout :: String -> [String] -> FFI.StructLayout
    createStructLayout name types =
        let fields = createFields types 0
            totalSize = sum (map FFI.fieldSize fields)
        in FFI.StructLayout name totalSize fields
    
    createFields :: [String] -> Int -> [FFI.FieldDef]
    createFields [] _ = []
    createFields (t:ts) offset =
        let btype = typeStringToBernCType t
            size = FFI.getFieldSize btype
            field = FFI.FieldDef ("field" ++ show offset) btype offset size
        in field : createFields ts (offset + size)
    
    typeStringToBernCType :: String -> FFI.BernCType
    typeStringToBernCType "Int" = FFI.BernInt
    typeStringToBernCType "Double" = FFI.BernDouble
    typeStringToBernCType "Bool" = FFI.BernBool
    typeStringToBernCType "Char" = FFI.BernChar
    typeStringToBernCType _ = FFI.BernVoid

interpretCommand mpos (Import moduleName) table = do
    -- Get the executable path to find the installation directory
    execPath <- getExecutablePath
    let installDir = takeDirectory execPath
    let installLibPath = installDir </> "lib" </> moduleName ++ ".brn"

    -- Try these paths in order:
    -- 1. lib/<name>.brn (relative to cwd)
    -- 2. <name>.brn (relative to cwd)
    -- 3. <install-dir>/lib/<name>.brn (installation directory)
    let localLibPath = "lib" </> moduleName ++ ".brn"
    let localPath = moduleName ++ ".brn"

    -- Check each path in order
    localLibExists <- doesFileExist localLibPath
    localExists <- doesFileExist localPath
    installLibExists <- doesFileExist installLibPath

    let path = if localLibExists 
               then localLibPath
               else if installLibExists
                    then installLibPath
                    else if localExists
                         then localPath
                         else ""

    if null path
        then die (posPrefix mpos ("Module not found: " ++ moduleName ++ " (searched in ./lib/, ./, and " ++ installDir </> "lib/)"))
        else do
            contents <- readFile path
            case parseBernFile path contents of
                Left err -> do
                    putStrLn $ errorBundlePretty err
                    die (posPrefix mpos ("Parse error in module " ++ moduleName ++ ":\n" ++ errorBundlePretty err))
                Right cmd -> do
                    interpretCommand mpos cmd table

interpretCommand mpos (Concat cmd1 cmd2) table = do
    table' <- interpretCommand mpos cmd1 table
    if hasReturn table'
        then return table'
        else interpretCommand mpos cmd2 table'

interpretCommand mpos (CForeignDecl name libPath argTypes retType) table = do    
    libPathVal <- case evaluate libPath table of
        Right v -> case valueToString v of
            Just s  -> return s
            Nothing -> die (posPrefix mpos "Library path must be a string or convertible to string")
        Left err -> die (posPrefix mpos err)

    result <- tryLoadFromLibs [libPathVal] name argTypes retType

    case result of
        Left err -> die $ posPrefix mpos ("Failed to bind C function " ++ name ++ ": " ++ err)
        Right wrapper -> do
            return $ insertHashtable table name (CBinding name wrapper)

-- Helper function to format source position prefix
posPrefix :: Maybe SourcePos -> String -> String
posPrefix (Just pos) msg = sourcePosPretty pos ++ ": " ++ msg
posPrefix Nothing msg = msg
    
tryLoadFromLibs :: [String] -> String -> [String] -> String -> IO (Either String ([Value] -> IO Value))
tryLoadFromLibs [] funcName _ _ = 
    return $ Left $ "Could not find function " ++ funcName ++ " in any library"
tryLoadFromLibs [libPath] funcName argTypes retType = do
    FFI.loadCFunction libPath funcName argTypes retType
tryLoadFromLibs (libPath:rest) funcName argTypes retType = do
    result <- FFI.loadCFunction libPath funcName argTypes retType
    case result of
        Right wrapper -> return $ Right wrapper
        Left _ -> tryLoadFromLibs rest funcName argTypes retType
        
-- Apply a function or lambda value to arguments
applyFunction :: Value -> [Value] -> Hashtable String Value -> Either String Value
applyFunction (Function clauses) args table = applyClauses clauses args table
applyFunction (Lambda clauses) args table = applyClauses clauses args table
applyFunction _ _ _ = Left "Called value is not a function"

applyClauses :: [Clause] -> [Value] -> Hashtable String Value -> Either String Value
applyClauses [] _ _ = Left "No matching function clause"
applyClauses (Clause patterns body : rest) args table =
    case matchAll patterns args of
        Nothing -> applyClauses rest args table
        Just bindings ->
            let newTable = foldl (\tbl (k,v) -> insertHashtable tbl k v) table bindings
            in case body of
                BodyExpr expr -> evaluate expr newTable
                BodyBlock cmd ->
                    -- Run block and look for __return; fall back to Undefined
                    let resultTable = unsafePerformIO (interpretCommand Nothing cmd newTable)
                    in case lookupHashtable resultTable "__return" of
                        Just v  -> Right v
                        Nothing -> Right Undefined

-- Match argument list against pattern list, returning bindings if successful
matchAll :: [Pattern] -> [Value] -> Maybe [(String, Value)]
matchAll ps vs
    | length ps /= length vs = Nothing
    | otherwise = fmap concat (sequence (zipWith matchOne ps vs))

matchOne :: Pattern -> Value -> Maybe [(String, Value)]
matchOne PWildcard _ = Just []
matchOne (PVar name) v = Just [(name, v)]
matchOne (PInt n) (Integer m) | n == m = Just []
matchOne (PDouble x) (Double y) | x == y = Just []
matchOne (PBool b) (Boolean c) | b == c = Just []
matchOne (PChar c) (Character d) | c == d = Just []
matchOne (PADT typeName pats) (AlgebraicDataType valType vals)
    | typeName == valType && length pats == length vals = matchAll pats vals
    | otherwise = Nothing
matchOne (PString s) (List vs _) | allCharacter vs && map (
    \(Character c) -> c) vs == s = Just []

-- List pattern: match empty list [] or list of patterns [a, b, c]
matchOne (PList []) (List [] _) = Just []
matchOne (PList pats) (List vals _)
    | length pats == length vals = matchAll pats vals
    | otherwise = Nothing

-- Cons pattern: [head|tail] matches non-empty list
matchOne (PCons headPat tailPat) (List (v:vs) _) = do
    headBindings <- matchOne headPat v
    tailBindings <- matchOne tailPat (List vs (length vs))
    return (headBindings ++ tailBindings)
matchOne (PCons _ _) (List [] _) = Nothing  -- Can't match cons on empty list

-- Set pattern: match empty set {} or set of patterns {a, b, c}
matchOne (PSet []) (Set [] _) = Just []
matchOne (PSet pats) (Set vals _)
    | length pats == length vals = matchAll pats vals
    | otherwise = Nothing

-- Set cons pattern: {head|tail} matches non-empty set
matchOne (PSetCons headPat tailPat) (Set (v:vs) _) = do
    headBindings <- matchOne headPat v
    tailBindings <- matchOne tailPat (Set vs (length vs))
    return (headBindings ++ tailBindings)
matchOne (PSetCons _ _) (Set [] _) = Nothing  -- Can't match cons on empty set

matchOne _ _ = Nothing

hasReturn :: Hashtable String Value -> Bool
hasReturn tbl = isJust (lookupHashtable tbl "__return")

-- Fatal error helper
die :: String -> IO a
die msg = putStrLn ("Error: " ++ msg) >> exitFailure

-- Convert iterable values (list, set, text) to a list of values for for-in loops
toIterable :: Value -> Either String [Value]
toIterable (List vals _) = Right vals
toIterable (Set vals _)  = Right vals
toIterable v@(List _ _) | Just s <- valueToString v = Right (map Character s)
toIterable _             = Left "Cannot iterate over this type"

-- Our 'evaluate' function receives an Expression and a Hashtable (for variables)
evaluate :: Expression -> Hashtable String Value -> Either String Value
-- If a Number is received, then it is itself.
evaluate (Number n) _ = (Right (Integer n))

evaluate (DoubleNum d) _ = (Right (Double d))

-- Boolean literal
evaluate (BoolLiteral b) _ = Right (Boolean b)

-- String literal
evaluate (StringLiteral s) _ = Right (List (map Character s) (length s))

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

-- Fmap expression: fmap collection function
evaluate (Fmap coll func) table = do
    collection <- evaluate coll table
    functionVal <- evaluate func table
    case collection of
        AlgebraicDataType typeName args -> do
            newArgs <- mapM (\arg -> applyFunction functionVal [arg] table) args
            Right (AlgebraicDataType typeName newArgs)
        List vals len -> do
            newVals <- mapM (\v -> applyFunction functionVal [v] table) vals
            Right (List newVals len)
        Set vals len -> do
            newVals <- mapM (\v -> applyFunction functionVal [v] table) vals
            Right (Set newVals len)
        _ -> Left "fmap() function can only be applied to ADTs, lists, or sets"
 
-- Index access: list[i], string[i], set[i], object["key"]
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
        (Object kvs, lst@(List _ _)) ->
            case valueToString lst of
                Just key -> case lookup key kvs of
                                Just v  -> Right v
                                Nothing -> Left ("Key not found: " ++ key)
                Nothing -> Left "Index must be integer or string (in case of objects)"
        (Object kvs, Integer i) ->
            let key = show i in
            case lookup key kvs of
                Just v  -> Right v
                Nothing -> Left ("Key not found: " ++ key)
        (Object kvs, Character c) ->
            case lookup [c] kvs of
                Just v  -> Right v
                Nothing -> Left ("Key not found: " ++ [c])
        _ -> Left "Cannot index this type or invalid index"

-- If a Variable name is received, then
evaluate (Variable name) table =
    -- Check if the name of the variable is on the Hashtable
    case lookupHashtable table name of
        -- If it is, then it is itself
        Just val -> Right val
        -- Otherwise, this variable was not defined.
        Nothing  -> Right Undefined

-- Read a file (not a command but an expression)
evaluate (ReadFile filenameExpr) table = do
    pathVal <- evaluate filenameExpr table
    case valueToString pathVal of
        Just path -> do
            let contents = unsafePerformIO (readFile path)
            return (List (map Character contents) (length contents))
        Nothing -> Left "File path must be a string"

evaluate GetHostMachine _ = 
    let hostMachine = os
    in Right (List (map Character hostMachine) (length hostMachine))

evaluate GetCurrentDir _ =
    let currentDir = unsafePerformIO (getCurrentDirectory)
    in Right (List (map Character currentDir) (length currentDir))

-- Carry source position for better error reporting
evaluate (WithPos pos expr) table =
    case evaluate expr table of
        Left err -> Left (sourcePosPretty pos ++ ": " ++ err)
        Right v  -> Right v

evaluate (AlgebraicDataTypeConstruct typeName args) table = do
    argVals <- mapM (`evaluate` table) args
    -- If the ADT is List, wrap in List constructor
    if typeName == "List" then
        Right (List argVals (length argVals))
    else if typeName == "Set" then
        Right (Set argVals (length argVals))
    else
        Right (AlgebraicDataType typeName argVals)

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

evaluate (ObjectLiteral kvExprs) table = do
    kvVals <- mapM (\(k, vExpr) -> do
                        vVal <- evaluate vExpr table
                        return (k, vVal)) kvExprs
    Right (Object kvVals)

evaluate (LambdaExpr params bodyExpr) _table =
    Right (Lambda [Clause params (BodyExpr bodyExpr)])
    
evaluate (FunctionCall name args) table =
    case lookupHashtable table name of
        Just (Function []) -> 
            case sequence (map (`evaluate` table) args) of
                Left err -> Left err
                Right vals -> Right $ AlgebraicDataType name vals
        Just (CBinding _ wrapper) -> 
            case sequence (map (`evaluate` table) args) of
                Left err -> Left err
                Right vals -> 
                    let result = unsafePerformIO (wrapper vals)
                    in Right result
        Just v -> 
            case sequence (map (`evaluate` table) args) of
                Left err -> Left err
                Right vals -> applyFunction v vals table
        Nothing -> Left "Function not found"

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
-- String-like (lists of chars) must come before numeric list cases
evalArith Add l1@(List _ _) l2@(List _ _)
    | Just s1 <- valueToString l1
    , Just s2 <- valueToString l2
    = Right (List (map Character (s1 ++ s2)) (length s1 + length s2))
evalArith Add l@(List _ _) (Character c)
    | Just s <- valueToString l
    = Right (List (map Character (s ++ [c])) (length s + 1))
evalArith Add (Character c) l@(List _ _)
    | Just s <- valueToString l
    = Right (List (map Character ([c] ++ s)) (1 + length s))
evalArith Add l@(List _ _) (Integer r)
    | Just s <- valueToString l
    = let sr = show r in Right (List (map Character (s ++ sr)) (length s + length sr))
evalArith Add (Integer l) r@(List _ _)
    | Just s <- valueToString r
    = let sl = show l in Right (List (map Character (sl ++ s)) (length sl + length s))
evalArith Add l@(List _ _) (Double r)
    | Just s <- valueToString l
    = let sr = show r in Right (List (map Character (s ++ sr)) (length s + length sr))
evalArith Add (Double l) r@(List _ _)
    | Just s <- valueToString r
    = let sl = show l in Right (List (map Character (sl ++ s)) (length sl + length s))
-- Map scalar addition over list elements (numeric lists)
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

-- Concatenation of List (String) with ADT (converted to string)
evalArith Add l@(List _ _) r@(AlgebraicDataType name vals)
    | Just s <- valueToString l
    = let sADT = prettyValue r
      in Right (List (map Character (s ++ sADT)) (length s + length sADT))
evalArith Add l@(AlgebraicDataType name vals) r@(List _ _)
    | Just s <- valueToString r
    = let sADT = prettyValue l
      in Right (List (map Character (sADT ++ s)) (length sADT + length s))

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
data IndexKey = IdxInt Int | IdxKey String deriving (Eq, Show)

setAt :: [IndexKey] -> Value -> Value -> Either String Value
setAt [] _ _ = Left "Invalid assignment target"
-- List update
setAt (IdxInt i:is) newVal (List vals len)
    | i < 0 || i >= len = Left "Index out of bounds"
    | null is =
        let old = vals !! i
        in if getValueType old == getValueType newVal
              then Right (List (replaceAt i newVal vals) len)
              else Left "Type error: assigned value type differs from list element type"
    | otherwise = do
        nested <- case vals !! i of
                    l@(List _ _) -> Right l
                    o@(Object _) -> Right o
                    _            -> Left "Type error: cannot index into non-collection"
        updatedNested <- setAt is newVal nested
        Right (List (replaceAt i updatedNested vals) len)
-- Object update
setAt (IdxKey k:is) newVal (Object kvs)
    | null is = Right (Object (upsert k newVal kvs))
    | otherwise = do
        nested <- case lookup k kvs of
                    Just v@(Object _) -> Right v
                    Just v@(List _ _) -> Right v
                    Just _            -> Left "Type error: cannot index into non-object/non-list"
                    Nothing           -> Left "Key not found for nested assignment"
        updatedNested <- setAt is newVal nested
        Right (Object (upsert k updatedNested kvs))
-- Type mismatch paths
setAt (IdxInt _ : _) _ (Object _) = Left "Type error: expected string key for object"
setAt (IdxKey _ : _) _ (List _ _) = Left "Type error: expected integer index for list"
setAt _ _ _ = Left "Type error: assignment target must be a list or object"

upsert :: String -> Value -> [(String, Value)] -> [(String, Value)]
upsert key val [] = [(key, val)]
upsert key val ((k,v):rest)
    | key == k  = (key, val) : rest
    | otherwise = (k,v) : upsert key val rest

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
evalComparison Equal v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 == s2))
evalComparison Equal (Character l) (Character r) = Right (Boolean (l == r))
evalComparison Equal (List l _) (List r _) = Right (Boolean (l == r))
evalComparison Equal (Set l _) (Set r _) = Right (Boolean (l == r))
-- (x != y)
evalComparison Different (Integer l) (Integer r) = Right (Boolean (l /= r))
evalComparison Different (Double l) (Double r) = Right (Boolean (l /= r))
evalComparison Different (Boolean l) (Boolean r) = Right (Boolean (l /= r))
evalComparison Different v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 /= s2))
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
evalComparison GreaterThan v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 > s2))
evalComparison GreaterThan (List l ls) (List r rs) = Right (Boolean (ls > rs))
evalComparison GreaterThan (Set l ls) (Set r rs) = Right (Boolean (ls > rs))

-- (x < y)
evalComparison LessThan (Integer l) (Integer r) = Right (Boolean (l < r))
evalComparison LessThan (Double l) (Double r) = Right (Boolean (l < r))
evalComparison LessThan v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 < s2))
evalComparison LessThan (List l ls) (List r rs) = Right (Boolean (ls < rs))
evalComparison LessThan (Set l ls) (Set r rs) = Right (Boolean (ls < rs))
-- (x >= y)
evalComparison GreaterThanEq (Integer l) (Integer r) = Right (Boolean (l >= r))
evalComparison GreaterThanEq (Double l) (Double r) = Right (Boolean (l >= r))
evalComparison GreaterThanEq v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 >= s2))
evalComparison GreaterThanEq (List l ls) (List r rs) = Right (Boolean (ls >= rs))
evalComparison GreaterThanEq (Set l ls) (Set r rs) = Right (Boolean (ls >= rs))
-- (x <= y)
evalComparison LessThanEq (Integer l) (Integer r) = Right (Boolean (l <= r))
evalComparison LessThanEq (Double l) (Double r) = Right (Boolean (l <= r))
evalComparison LessThanEq v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 <= s2))
evalComparison LessThanEq (List l ls) (List r rs) = Right (Boolean (ls <= rs))
evalComparison LessThanEq (Set l ls) (Set r rs) = Right (Boolean (ls <= rs))

-- In case of an error
evalComparison _ _ _ = Left "Type error in binary operation"

-- Evaluate unary operations
evalUnaryOp :: UnaryOperation -> Value -> Either String Value
evalUnaryOp Negate (Integer n) = Right (Integer (-n))
evalUnaryOp Negate (Double d) = Right (Double (-d))
evalUnaryOp Not (Boolean b) = Right (Boolean (not b))
evalUnaryOp TypeOf v =
    let t = getValueType v
    in Right (List (map Character t) (length t))
evalUnaryOp SizeOf v
    | Just s <- valueToString v = Right (Integer (length s))
evalUnaryOp SizeOf (List _ len) = Right (Integer len)
evalUnaryOp SizeOf (Set _ len) = Right (Integer len)
evalUnaryOp SizeOf (Object kvs) = Right (Integer (length kvs))
evalUnaryOp _ _ = Left "Type error in unary operation"

evalUnion :: BinaryOperation -> Value -> Value -> Either String Value
-- (a <> b)
evalUnion Concatenation (List l1 len1) (List l2 len2)
    | allCharacter l1 && allCharacter l2 =
        let combined = l1 ++ l2
        in Right (List combined (len1 + len2))
    | allSameType (l1 ++ l2) = Right (List (l1 ++ l2) (len1 + len2))
    | otherwise = Left "Type error: list concatenation requires elements of the same type"
evalUnion Concatenation (Set s1 len1) (Set s2 len2) =
    let combined = s1 ++ filter (`notElem` s1) s2
    in Right (Set combined (length combined))

evalUnion Concatenation v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2
    = let combined = s1 ++ s2
      in Right (List (map Character combined) (length combined))
evalUnion Concatenation (Character c1) (Character c2) =
    Right (List [Character c1, Character c2] 2)

-- (a <| b) - Union
evalUnion Union (Set s1 len1) (Set s2 len2) =
    let combined = s1 ++ filter (`notElem` s1) s2
    in Right (Set combined (length combined))
evalUnion Union (List l1 len1) (List l2 len2) =
    let combined = l1 ++ filter (`notElem` l1) l2
    in Right (List combined (length combined))
evalUnion Union v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 =
        let combinedStr = s1 ++ filter (`notElem` s1) s2
        in Right (List (map Character combinedStr) (length combinedStr))

-- (a |> b) - Intersection
evalUnion Intersection (Set s1 len1) (Set s2 len2) =
    let common = filter (`elem` s2) s1
    in Right (Set common (length common))
evalUnion Intersection (List l1 len1) (List l2 len2) =
    let common = filter (`elem` l2) l1
    in Right (List common (length common))
evalUnion Intersection v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 =
        let commonChars = filter (`elem` s2) s1
        in Right (List (map Character commonChars) (length commonChars))

-- (a </> b) - Difference
evalUnion Difference (Set s1 len1) (Set s2 len2) =
    let diff = filter (`notElem` s2) s1
    in Right (Set diff (length diff))
evalUnion Difference (List l1 len1) (List l2 len2) =
    let diff = filter (`notElem` l2) l1
    in Right (List diff (length diff))
evalUnion Difference v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 =
        let diffChars = filter (`notElem` s2) s1
        in Right (List (map Character diffChars) (length diffChars))

evalUnion _ _ _ = Left "Type error in Array operation"

-- Pretty print a value (without type constructor names)
prettyValue :: Value -> String
prettyValue (Integer n) = show n
prettyValue (Double d) = show d
prettyValue NaN = "NaN"
prettyValue Undefined = "undefined"
prettyValue (Boolean True) = "true"
prettyValue (Boolean False) = "false"
prettyValue v | Just s <- valueToString v = s
prettyValue (Character c) = [c]
prettyValue (List vals _) = "[" ++ intercalateWith ", " (map prettyValue vals) ++ "]"
prettyValue (Set vals _) = "{" ++ intercalateWith ", " (map prettyValue vals) ++ "}"
prettyValue (Object pairs) = "#{" ++ intercalateWith ", " (map (\(k,v) -> k ++ ": " ++ prettyValue v) pairs) ++ "}#"
prettyValue (AlgebraicDataType name vals) = name ++ "(" ++ intercalateWith ", " (map prettyValue vals) ++ ")"
prettyValue (Function _) = "<function>"
prettyValue (Lambda _) = "<lambda>"

-- Helper for joining strings
intercalateWith :: String -> [String] -> String
intercalateWith _ [] = ""
intercalateWith _ [x] = x
intercalateWith sep (x:xs) = x ++ sep ++ intercalateWith sep xs

