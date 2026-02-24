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
import Text.Megaparsec.Pos (sourceName, sourceLine, sourceColumn, unPos)
import Data.List (isInfixOf, nub, isPrefixOf)
import qualified Language.FFI as FFI
import Control.Concurrent.MVar
import System.Mem.StableName

-- Global mutable reference to the global scope using MVar
{-# NOINLINE globalScopeMVar #-}
globalScopeMVar :: MVar (Hashtable String Value)
globalScopeMVar = unsafePerformIO $ do
    newMVar emptyHashtable

-- | Error type for better error messages
data EvalError = EvalError
    { errorPos :: Maybe SourcePos
    , errorMsg :: String
    , errorHint :: Maybe String
    }

-- | Format error message
formatError :: EvalError -> String
formatError (EvalError mpos msg mhint) =
    let redBold = "\x1b[1;31m"
        red = "\x1b[31m"
        yellow = "\x1b[33m"
        cyan = "\x1b[36m"
        magenta = "\x1b[35m"
        dim = "\x1b[2m"
        reset = "\x1b[0m"
        hintStr = case mhint of
                    Just h -> "\n\n" ++ yellow ++ "Hint: " ++ reset ++ h
                    Nothing -> ""
    in case mpos of
        Just pos ->
            let fname = sourceName pos
                ln = show (unPos (sourceLine pos))
                col = show (unPos (sourceColumn pos))
                header = redBold ++ "[bern]" ++ redBold ++ " An error was found while executing the script: " ++ reset ++ cyan ++ fname ++ reset
                body = "Error" ++ reset ++ " at " ++ magenta ++ ln ++ ":" ++ col ++ reset ++ ": " ++ red ++ msg ++ reset
            in header ++ "\n" ++ body ++ hintStr
        Nothing ->
            redBold ++ "Error: " ++ reset ++ red ++ msg ++ reset ++ hintStr

-- | Create simple EvalError
mkEvalError :: Maybe SourcePos -> String -> EvalError
mkEvalError mpos msg = EvalError mpos msg Nothing

-- | Create EvalError with hint
mkEvalErrorHint :: Maybe SourcePos -> String -> String -> EvalError
mkEvalErrorHint mpos msg hint = EvalError mpos msg (Just hint)

stringToValue :: String -> Value
stringToValue s = List (map Character s) (length s)

importOwnerKey :: String -> String
importOwnerKey name = "__bern_import_owner_" ++ name

importConflictKey :: String -> String
importConflictKey name = "__bern_import_conflict_" ++ name

importConflictHintKey :: String -> String
importConflictHintKey name = "__bern_import_conflict_hint_" ++ name

importModuleSymbolsKey :: String -> String
importModuleSymbolsKey moduleName = "__bern_import_symbols_" ++ moduleName

isMetaSymbolName :: String -> Bool
isMetaSymbolName name = "__" `isPrefixOf` name || ':' `elem` name

collectImportSymbols :: Command -> [String]
collectImportSymbols cmd = nub (filter (not . isMetaSymbolName) (go cmd))
  where
    go Skip = []
    go (Concat c1 c2) = go c1 ++ go c2
    go (FunctionDef name _) = [name]
    go (Assign name _) = [name]
    go (GlobalAssign name _) = [name]
    go (AlgebraicTypeDef (ADTDef _ typeName constructors)) =
        typeName : [ctorName | ADTConstructor ctorName _ <- constructors]
    go (CForeignDecl name _ _ _) = [name]
    go _ = []

registerImportedSymbol :: String -> String -> Hashtable String Value -> Hashtable String Value
registerImportedSymbol moduleName symbol table =
    case lookupHashtable table (importOwnerKey symbol) of
        Nothing ->
            insertHashtable table (importOwnerKey symbol) (stringToValue moduleName)
        Just ownerVal ->
            case valueToString ownerVal of
                Just ownerModule
                    | ownerModule /= moduleName ->
                        let tableWithConflict = insertHashtable table (importConflictKey symbol) (Boolean True)
                            hint = "symbol '" ++ symbol ++ "' exists in both '" ++ ownerModule ++ "' and '" ++ moduleName ++ "'"
                        in insertHashtable tableWithConflict (importConflictHintKey symbol) (stringToValue hint)
                _ -> table

isAmbiguousImportedSymbol :: Hashtable String Value -> String -> Bool
isAmbiguousImportedSymbol table name =
    case lookupHashtable table (importConflictKey name) of
        Just (Boolean True) -> True
        _                   -> False

ambiguousImportedSymbolError :: Hashtable String Value -> String -> EvalError
ambiguousImportedSymbolError table name =
    let detail =
            case lookupHashtable table (importConflictHintKey name) of
                Just v ->
                    case valueToString v of
                        Just s  -> s ++ ". Use a qualified call like '<module>:" ++ name ++ "(...)'."
                        Nothing -> "Use a qualified call like '<module>:" ++ name ++ "(...)'."
                Nothing -> "Use a qualified call like '<module>:" ++ name ++ "(...)'."
    in mkEvalErrorHint Nothing ("ambiguous imported symbol '" ++ name ++ "'") detail

splitQualifiedName :: String -> Maybe (String, String)
splitQualifiedName name =
    case break (== ':') name of
        (moduleName, ':' : symbol)
            | not (null moduleName) && not (null symbol) -> Just (moduleName, symbol)
        _ -> Nothing

withQualifiedModuleScope :: Hashtable String Value -> String -> Hashtable String Value
withQualifiedModuleScope table moduleName =
    case lookupHashtable table (importModuleSymbolsKey moduleName) of
        Just (Object symbols) ->
            foldl
                (\tbl (symbol, _) ->
                    let maybeQualified = lookupHashtable table (moduleName ++ ":" ++ symbol)
                        withSymbol = case maybeQualified of
                            Just value -> insertHashtable tbl symbol value
                            Nothing    -> tbl
                    in insertHashtable withSymbol (importConflictKey symbol) (Boolean False))
                table
                symbols
        _ -> table

interpretCommand :: Maybe SourcePos -> Command -> Hashtable String Value -> IO (Hashtable String Value)
interpretCommand mpos Skip table = return table

interpretCommand mpos (Input varName promptExpr) table = do
    promptVal <- case evaluate promptExpr table of
                    Right v -> case valueToString v of
                                  Just s  -> return s
                                  Nothing -> die (mkEvalErrorHint mpos 
                                      "expected String for prompt"
                                      "the prompt expression must evaluate to a string")
                    Left err -> die err
    putStr promptVal
    input <- getLine
    let newTable = insertHashtable table varName (List (map Character input) (length input))
    return newTable

interpretCommand mpos (WriteFile filePath contentExpr) table = do
    pathVal <- case evaluate filePath table of
                    Right v -> case valueToString v of
                                  Just s  -> return s
                                  Nothing -> die (mkEvalErrorHint mpos
                                      "expected String for file path"
                                      "file paths must be strings")
                    Left err -> die err
    contentVal <- case evaluate contentExpr table of
                    Right v -> case valueToString v of
                                  Just s  -> return s
                                  Nothing -> die (mkEvalErrorHint mpos
                                      "expected String for file content"
                                      "content to write must be a string")
                    Left err -> die err
    writeFile pathVal contentVal
    return table

interpretCommand mpos (Print expr) table =
    case evaluate expr table of
        Right Undefined -> return table
        Right val -> putStrLn (prettyValue val) >> return table
        Left err  -> die err

interpretCommand mpos (Assign name expr) table =
    case evaluate expr table of
        Right v -> return (insertHashtable table name v)
        Left err -> die err

interpretCommand mpos (GlobalAssign name expr) table =
    case evaluate expr table of
        Right v@(Integer _)   -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(Double _)    -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(Boolean _)   -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(Character _) -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(List _ _)    -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(Set _ _)     -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(Object _)    -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(Function _)  -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(Lambda _)    -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(AlgebraicDataType _ _) -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(CBinding _ _) -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v@(NaN)          -> do
            globalScope <- takeMVar globalScopeMVar
            let newGlobalScope = insertHashtable globalScope name v
            putMVar globalScopeMVar newGlobalScope
            return (insertHashtable table name v)
        Right v               -> die (mkEvalErrorHint mpos
            ("cannot assign unexpected value to '" ++ name ++ "'")
            ("value type: " ++ show v))
        Left err              -> die err

interpretCommand mpos (AssignIndex name idxExprs expr) table = do
    baseVal <- case lookupHashtable table name of
                  Just v  -> return v
                  Nothing -> die (mkEvalErrorHint mpos
                      ("undefined variable '" ++ name ++ "'")
                      "variables must be defined before indexing")
    idxVals <- mapM (\ie -> case evaluate ie table of
                              Right (Integer n)      -> return (IdxInt n)
                              Right (Character c)    -> return (IdxKey [c])
                              Right l@(List _ _)     ->
                                  case valueToString l of
                                    Just s  -> return (IdxKey s)
                                    Nothing -> die (mkEvalError mpos
                                        "index must be Int or String")
                              Right _                -> die (mkEvalError mpos
                                        "index must be Int or String")
                              Left err               -> die err) idxExprs
    newVal <- case evaluate expr table of
                Right v  -> return v
                Left err -> die err
    case setAt idxVals newVal baseVal of
        Right updated -> return (insertHashtable table name updated)
        Left err      -> die err

interpretCommand mpos (Conditional cond thenCmd elseCmd) table =
    case evaluate cond table of
        Right (Boolean True)  -> interpretCommand mpos thenCmd table
        Right (Boolean False) -> interpretCommand mpos elseCmd table
        Right v               -> die (mkEvalErrorHint mpos
            "condition must be Bool"
            ("found: " ++ getValueType v))
        Left err              -> die err

interpretCommand mpos (Repeat count cmd) table =
    case evaluate count table of
        Right (Integer n) -> loop n table
        Right v           -> die (mkEvalErrorHint mpos
            "repeat count must be Int"
            ("found: " ++ getValueType v))
        Left err          -> die err
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
                Right v               -> die (mkEvalErrorHint mpos
                    "while condition must be Bool"
                    ("found: " ++ getValueType v))
                Left err              -> die err
    in loop table

interpretCommand mpos (ForIn varName collection cmd) table =
    case evaluate collection table of
        Right coll ->
            case toIterable table coll of
                Right vals -> loop vals table
                Left err   -> die err
        Left err -> die err
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
            case toIterable table coll of
                Right vals -> loop (zip vals [0..]) table
                Left err   -> die err
        Left err -> die err
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
        Left err -> die err

interpretCommand mpos (AlgebraicTypeDef (ADTDef isIterative typeName constructors)) table = do
    let adtVal = AlgebraicDataType typeName []
    let newTable = insertHashtable table typeName adtVal
    let tableWithCtors = foldl (\tbl (ADTConstructor ctorName _) ->
                                  insertHashtable tbl ctorName (Function [])) newTable constructors
    let tableWithIterableFlags = foldl (\tbl (ADTConstructor ctorName _) ->
                                          insertHashtable tbl (iterableCtorKey ctorName) (Boolean isIterative))
                                       tableWithCtors
                                       constructors
    
    case constructors of
        [ADTConstructor _ fieldTypes] -> do
            let fieldTypeNames = map typeToString fieldTypes
            let layout = createStructLayout typeName fieldTypeNames
            FFI.registerStruct layout
        _ -> return () 
    
    return tableWithIterableFlags
  where
    iterableCtorKey :: String -> String
    iterableCtorKey ctorName = "__bern_iterable_adt_ctor_" ++ ctorName

    typeToString :: Type -> String
    typeToString TInt = "Int"
    typeToString TDouble = "Double"
    typeToString TBool = "Bool"
    typeToString TChar = "Char"
    typeToString TString = "String"
    typeToString TList = "List"
    typeToString TSet = "Set"
    typeToString (TCustom "Byte") = "Byte"
    typeToString (TCustom "UChar") = "UChar"
    typeToString (TCustom "Float") = "Float"
    typeToString (TCustom "Short") = "Short"
    typeToString (TCustom "UShort") = "UShort"
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
    typeStringToBernCType "Byte" = FFI.BernByte
    typeStringToBernCType "UChar" = FFI.BernByte
    typeStringToBernCType "Float" = FFI.BernFloat
    typeStringToBernCType "Short" = FFI.BernShort
    typeStringToBernCType "UShort" = FFI.BernUShort
    typeStringToBernCType "Ptr" = FFI.BernPtr
    typeStringToBernCType "Pointer" = FFI.BernPtr
    typeStringToBernCType _ = FFI.BernVoid

interpretCommand mpos (Import moduleName mAlias) table = do
    execPath <- getExecutablePath
    let installDir = takeDirectory execPath
    let qualifier = case mAlias of
            Just alias -> alias
            Nothing    -> moduleName
    let installLibPath = installDir </> "lib" </> moduleName ++ ".brn"
    let localLibPath = "lib" </> moduleName ++ ".brn"
    let localPath = moduleName ++ ".brn"

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
        then die (mkEvalErrorHint mpos
            ("module '" ++ moduleName ++ "' not found")
            ("searched in: ./lib/, ./, and " ++ installDir </> "lib/"))
        else do
            contents <- readFile path
            case parseBernFile path contents of
                Left err -> do
                    putStrLn $ errorBundlePretty err
                    die (mkEvalErrorHint mpos
                        ("failed to parse module '" ++ moduleName ++ "'")
                        "see parse errors above")
                Right cmd -> do
                    let symbols = collectImportSymbols cmd
                    let importBaseTable =
                            foldl (\tbl symbol -> insertHashtable tbl symbol Undefined) table symbols
                    importedTable <- interpretCommand mpos cmd importBaseTable
                    let tableWithQualified = foldl
                            (\tbl symbol ->
                                case lookupHashtable importedTable symbol of
                                    Just value -> insertHashtable tbl (qualifier ++ ":" ++ symbol) value
                                    Nothing    -> tbl)
                            importedTable
                            symbols
                    let tableWithImportMeta = foldl
                            (\tbl symbol -> registerImportedSymbol qualifier symbol tbl)
                            tableWithQualified
                            symbols
                    let tableWithModuleSymbols =
                            insertHashtable tableWithImportMeta
                                (importModuleSymbolsKey qualifier)
                                (Object [(symbol, Boolean True) | symbol <- symbols])
                    return tableWithModuleSymbols

interpretCommand mpos (Concat cmd1 cmd2) table = do
    table' <- interpretCommand mpos cmd1 table
    if hasReturn table'
        then return table'
        else interpretCommand mpos cmd2 table'

interpretCommand mpos (CForeignDecl name libPath argTypes retType) table = do    
    libPathVal <- case evaluate libPath table of
        Right v -> case valueToString v of
            Just s  -> return s
            Nothing -> die (mkEvalError mpos "library path must be String")
        Left err -> die err

    result <- tryLoadFromLibs [libPathVal] name argTypes retType

    case result of
        Left err -> die (mkEvalErrorHint mpos
            ("failed to bind C function '" ++ name ++ "'")
            err)
        Right wrapper -> do
            return $ insertHashtable table name (CBinding name wrapper)
    
tryLoadFromLibs :: [String] -> String -> [String] -> String -> IO (Either String ([Value] -> IO Value))
tryLoadFromLibs [] funcName _ _ = 
    return $ Left $ "function not found in any library"
tryLoadFromLibs [libPath] funcName argTypes retType = do
    FFI.loadCFunction libPath funcName argTypes retType
tryLoadFromLibs (libPath:rest) funcName argTypes retType = do
    result <- FFI.loadCFunction libPath funcName argTypes retType
    case result of
        Right wrapper -> return $ Right wrapper
        Left _ -> tryLoadFromLibs rest funcName argTypes retType
        
-- Apply a function or lambda value to arguments
applyFunction :: Value -> [Value] -> Hashtable String Value -> Either EvalError Value
applyFunction (Function clauses) args table = applyClauses clauses args table
applyFunction (Lambda clauses) args table = applyClauses clauses args table
applyFunction v _ _ = Left $ mkEvalError Nothing ("expected function, found " ++ getValueType v)

applyClauses :: [Clause] -> [Value] -> Hashtable String Value -> Either EvalError Value
applyClauses [] args _ = Left $ mkEvalError Nothing ("no matching pattern for " ++ show (length args) ++ " argument(s)")
applyClauses (Clause patterns body : rest) args table =
    case matchAll patterns args of
        Nothing -> applyClauses rest args table
        Just bindings ->
            let newTable = foldl (\tbl (k,v) -> insertHashtable tbl k v) table bindings
            in case body of
                        BodyExpr expr -> evaluate expr newTable
                        BodyBlock cmd ->
                            let resultTable = unsafePerformIO (interpretCommand Nothing cmd newTable)
                            in case lookupHashtable resultTable "__return" of
                                Just v  -> Right v
                                Nothing -> Right Undefined

len :: [a] -> Int
len = length

-- Match argument list against pattern list, returning bindings if successful
matchAll :: [Pattern] -> [Value] -> Maybe [(String, Value)]
matchAll ps vs
    | length ps /= len vs = Nothing
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
matchOne (PString s) (List vs _) | allCharacter vs && map (\(Character c) -> c) vs == s = Just []
matchOne (PList []) (List [] _) = Just []
matchOne (PList pats) (List vals _)
    | length pats == length vals = matchAll pats vals
    | otherwise = Nothing
matchOne (PCons headPat tailPat) (List (v:vs) _) = do
    headBindings <- matchOne headPat v
    tailBindings <- matchOne tailPat (List vs (length vs))
    return (headBindings ++ tailBindings)
matchOne (PCons _ _) (List [] _) = Nothing
matchOne (PSet []) (Set [] _) = Just []
matchOne (PSet pats) (Set vals _)
    | length pats == length vals = matchAll pats vals
    | otherwise = Nothing
matchOne (PSetCons headPat tailPat) (Set (v:vs) _) = do
    headBindings <- matchOne headPat v
    tailBindings <- matchOne tailPat (Set vs (length vs))
    return (headBindings ++ tailBindings)
matchOne (PSetCons _ _) (Set [] _) = Nothing
matchOne _ _ = Nothing

hasReturn :: Hashtable String Value -> Bool
hasReturn tbl = isJust (lookupHashtable tbl "__return")

-- | Fatal error helper now accepts structured `EvalError`.
die :: EvalError -> IO a
die evalErr = putStrLn (formatError evalErr) >> exitFailure

-- Convert iterable values (list, set, and explicitly iterative ADTs) to a list of values for for-in loops
toIterable :: Hashtable String Value -> Value -> Either EvalError [Value]
toIterable table v@(AlgebraicDataType ctorName vals)
    | isIterativeCtor table ctorName = Right vals
    | otherwise = Left $ mkEvalError Nothing ("cannot iterate over " ++ getValueType v ++ ". Have you defined it with 'adt iterative'?")
toIterable _ (List vals _) = Right vals
toIterable _ (Set vals _)  = Right vals
toIterable _ v = Left $ mkEvalError Nothing ("cannot iterate over " ++ getValueType v)

isIterativeCtor :: Hashtable String Value -> String -> Bool
isIterativeCtor tbl ctorName =
    case lookupHashtable tbl ("__bern_iterable_adt_ctor_" ++ ctorName) of
        Just (Boolean True) -> True
        _                   -> False


evaluate :: Expression -> Hashtable String Value -> Either EvalError Value
evaluate (Number n) _ = Right (Integer n)
evaluate (DoubleNum d) _ = Right (Double d)
evaluate (BoolLiteral b) _ = Right (Boolean b)
evaluate (IfExpr cond thenExpr elseExpr) table = do
    condVal <- evaluate cond table
    case condVal of
        Boolean True  -> evaluate thenExpr table
        Boolean False -> evaluate elseExpr table
        v -> Left $ mkEvalErrorHint Nothing
            "condition must be Bool"
            ("found: " ++ getValueType v)
evaluate (CaseExpr target branches) table = do
    targetVal <- evaluate target table
    evalBranches targetVal branches
  where
    evalBranches :: Value -> [CaseBranch] -> Either EvalError Value
    evalBranches _ [] = Left $ mkEvalError Nothing "no matching case branch"
    evalBranches v (CaseBranch pat body : rest) =
        case matchOne pat v of
            Just bindings ->
                let caseTable = foldl (\tbl (k,val) -> insertHashtable tbl k val) table bindings
                in evaluate body caseTable
            Nothing -> evalBranches v rest
evaluate (StringLiteral s) _ = Right (List (map Character s) (length s))
evaluate (CharLiteral c) _ = Right (Character c)

evaluate (ListLiteral exprs) table = do
    vals <- mapM (`evaluate` table) exprs
    if null vals 
        then Right (List [] 0)
        else if allSameType vals
            then Right (List vals (length vals))
            else Left $ mkEvalError Nothing "list elements must have the same type"

evaluate (SetLiteral exprs) table = do
    vals <- mapM (`evaluate` table) exprs
    Right (Set vals (length vals))

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
        _ -> Left $ mkEvalError Nothing "range bounds must be numeric"

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
        _ -> Left $ mkEvalError Nothing ("fmap requires List, Set, or ADT, found " ++ getValueType collection)
 
evaluate (Index expr idxExpr) table = do
    collection <- evaluate expr table
    idx <- evaluate idxExpr table
    case (collection, idx) of
        (List vals len, Integer i) 
            | i >= 0 && i < len -> Right (vals !! i)
            | otherwise -> Left $ mkEvalError Nothing ("index " ++ show i ++ " out of bounds (length " ++ show len ++ ")")
        (Set vals len, Integer i)
            | i >= 0 && i < len -> Right (vals !! i)
            | otherwise -> Left $ mkEvalError Nothing ("index " ++ show i ++ " out of bounds (length " ++ show len ++ ")")
        (Object kvs, lst@(List _ _)) ->
            case valueToString lst of
                Just key -> case lookup key kvs of
                                Just v  -> Right v
                                Nothing -> Left $ mkEvalError Nothing ("key '" ++ key ++ "' not found")
                Nothing -> Left $ mkEvalError Nothing "object index must be String"
        (Object kvs, Integer i) ->
            let key = show i in
            case lookup key kvs of
                Just v  -> Right v
                Nothing -> Left $ mkEvalError Nothing ("key '" ++ key ++ "' not found")
        (Object kvs, Character c) ->
            case lookup [c] kvs of
                Just v  -> Right v
                Nothing -> Left $ mkEvalError Nothing ("key '" ++ [c] ++ "' not found")
        _ -> Left $ mkEvalError Nothing ("cannot index " ++ getValueType collection ++ " with " ++ getValueType idx)

evaluate (Variable name) table
    | isAmbiguousImportedSymbol table name = Left (ambiguousImportedSymbolError table name)
    | otherwise = unsafePerformIO $ do
    globalScopeNow <- readMVar globalScopeMVar
    case lookupHashtable globalScopeNow name of
        Just val -> do
            return (Right val)
        Nothing  ->
            case lookupHashtable table name of
                Just val -> do
                    return (Right val)
                Nothing  -> do
                    return (Right Undefined)

evaluate (ReadFile filenameExpr) table = do
    pathVal <- evaluate filenameExpr table
    case valueToString pathVal of
        Just path -> do
            let contents = unsafePerformIO (readFile path)
            return (List (map Character contents) (length contents))
        Nothing -> Left $ mkEvalError Nothing "file path must be String"

evaluate GetHostMachine _ = 
    let hostMachine = os
    in Right (List (map Character hostMachine) (length hostMachine))

evaluate GetCurrentDir _ =
    let currentDir = unsafePerformIO (getCurrentDirectory)
    in Right (List (map Character currentDir) (length currentDir))

evaluate (WithPos pos expr) table =
    case evaluate expr table of
        Left err ->
            case errorPos err of
                Nothing -> Left (err { errorPos = Just pos })
                Just _  -> Left err
        Right v  -> Right v

evaluate (AlgebraicDataTypeConstruct typeName args) table = do
    argVals <- mapM (`evaluate` table) args
    if typeName == "List" then
        Right (List argVals (length argVals))
    else if typeName == "Set" then
        Right (Set argVals (length argVals))
    else
        Right (AlgebraicDataType typeName argVals)

evaluate (BinaryOperator op left right) table = do
    leftVal <- evaluate left table
    rightVal <- evaluate right table
    if isArithmetic op
        then evalArith op leftVal rightVal
        else if isUnion op
            then evalUnion op leftVal rightVal
            else evalComparison op leftVal rightVal

evaluate (UnaryOperator op expr) table = do
    val <- evaluate expr table
    evalUnaryOp op val

evaluate (ObjectLiteral kvExprs) table = do
    kvVals <- mapM (\(k, vExpr) -> do
                        vVal <- evaluate vExpr table
                        return (k, vVal)) kvExprs
    Right (Object kvVals)

evaluate (LambdaExpr params bodyExpr) _table =
    Right (Lambda [Clause params (BodyExpr bodyExpr)])
    
evaluate (FunctionCall name args) table
    | isAmbiguousImportedSymbol table name = Left (ambiguousImportedSymbolError table name)
    | otherwise =
        case lookupHashtable table name of
            Nothing -> Left $ mkEvalError Nothing ("undefined function with name '" ++ name ++ "'")
            Just fnVal ->
                case sequence (map (`evaluate` table) args) of
                    Left err -> Left err
                    Right vals ->
                        let callTable =
                                case splitQualifiedName name of
                                    Just (moduleName, baseSymbol) ->
                                        let moduleScoped = withQualifiedModuleScope table moduleName
                                            withBase = insertHashtable moduleScoped baseSymbol fnVal
                                        in insertHashtable withBase (importConflictKey baseSymbol) (Boolean False)
                                    Nothing -> table
                        in dispatchCall name fnVal vals callTable
  where
    dispatchCall callName (Function []) vals _ =
        let ctorName = case splitQualifiedName callName of
                        Just (_, base) -> base
                        Nothing        -> callName
        in Right (AlgebraicDataType ctorName vals)
    dispatchCall _ (CBinding _ wrapper) vals _ =
        Right (unsafePerformIO (wrapper vals))
    dispatchCall _ fnVal vals callTable =
        applyFunction fnVal vals callTable

evaluate _ _ = Left $ mkEvalError Nothing "unsupported expression"

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

evalArith :: BinaryOperation -> Value -> Value -> Either EvalError Value
evalArith Add (Integer l) (Integer r) = Right (Integer (l + r))
evalArith Add (Double l) (Double r) = Right (Double (l + r))
evalArith Add (Integer l) (Double r) = Right (Double (fromIntegral l + r))
evalArith Add (Double l) (Integer r) = Right (Double (l + fromIntegral r))
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
evalArith Add (List l1 len1) (List l2 len2) =
    if len1 /= len2
        then Left $ mkEvalError Nothing ("cannot add lists of different lengths (" ++ show len1 ++ " and " ++ show len2 ++ ")")
        else let combined = zipWith numericAdd l1 l2
             in if all isRight combined
                   then Right (List (map fromRight combined) len1)
                   else Left $ mkEvalError Nothing "list addition requires numeric elements"
  where
    isRight (Right _) = True
    isRight _        = False
    fromRight (Right v) = v
    fromRight _         = error "Unexpected Left value"
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

evalArith Subtract (Integer l) (Integer r) = Right (Integer (l - r))
evalArith Subtract (Double l) (Double r) = Right (Double (l - r))
evalArith Subtract (Integer l) (Double r) = Right (Double (fromIntegral l - r))
evalArith Subtract (Double l) (Integer r) = Right (Double (l - fromIntegral r))
evalArith Subtract (List vals len) scalar@(Integer _) = do
    vals' <- mapM (\v -> numericSub v scalar) vals
    Right (List vals' len)
evalArith Subtract (List vals len) scalar@(Double _) = do
    vals' <- mapM (\v -> numericSub v scalar) vals
    Right (List vals' len)
evalArith Subtract scalar@(Integer _) (List vals len) = do
    vals' <- mapM (numericSub scalar) vals
    Right (List vals' len)
evalArith Subtract scalar@(Double _) (List vals len) = do
    vals' <- mapM (numericSub scalar) vals
    Right (List vals' len)
evalArith Subtract (List l1 len1) (List l2 len2) =
    if len1 /= len2
        then Left $ mkEvalError Nothing ("cannot subtract lists of different lengths (" ++ show len1 ++ " and " ++ show len2 ++ ")")
        else let combined = zipWith numericSub l1 l2
             in if all isRight combined
                   then Right (List (map fromRight combined) len1)
                   else Left $ mkEvalError Nothing "list subtraction requires numeric elements"
  where
    isRight (Right _) = True
    isRight _        = False
    fromRight (Right v) = v
    fromRight _         = error "Unexpected Left value"
evalArith Subtract (Set s1 len1) v =
    if v `elem` s1
        then Right (Set (filter (/= v) s1) (len1 - 1))
        else Right (Set s1 len1)

evalArith Multiply (Integer l) (Integer r) = Right (Integer (l * r))
evalArith Multiply (Double l) (Double r) = Right (Double (l * r))
evalArith Multiply (Integer l) (Double r) = Right (Double (fromIntegral l * r))
evalArith Multiply (Double l) (Integer r) = Right (Double (l * fromIntegral r))
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
        then Left $ mkEvalError Nothing ("cannot multiply lists of different lengths (" ++ show len1 ++ " and " ++ show len2 ++ ")")
        else let combined = zipWith numericMul l1 l2
             in if all isRight combined
                   then Right (List (map fromRight combined) len1)
                   else Left $ mkEvalError Nothing "list multiplication requires numeric elements"
  where
    isRight (Right _) = True
    isRight _        = False
    fromRight (Right v) = v
    fromRight _         = error "Unexpected Left value"

evalArith Divide (Integer l) (Integer r)
    | r == 0    = Right NaN
    | otherwise = Right (Integer (l `div` r))
evalArith Divide (Double l) (Double r)
    | r == 0.0  = Right NaN
    | otherwise = Right (Double (l / r))
evalArith Divide (Integer l) (Double r)
    | r == 0.0  = Right NaN
    | otherwise = Right (Double (fromIntegral l / r))
evalArith Divide (Double l) (Integer r)
    | r == 0    = Right NaN
    | otherwise = Right (Double (l / fromIntegral r))
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
        then Left $ mkEvalError Nothing ("cannot divide lists of different lengths (" ++ show len1 ++ " and " ++ show len2 ++ ")")
        else let combined = zipWith numericDiv l1 l2
             in if all isRight combined
                   then Right (List (map fromRight combined) len1)
                   else Left $ mkEvalError Nothing "list division requires numeric elements"
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

evalArith op l r = Left $ mkEvalError Nothing ("cannot apply " ++ show op ++ " to " ++ getValueType l ++ " and " ++ getValueType r)

numericAdd :: Value -> Value -> Either EvalError Value
numericAdd (Integer l) (Integer r) = Right (Integer (l + r))
numericAdd (Double l) (Double r) = Right (Double (l + r))
numericAdd (Integer l) (Double r) = Right (Double (fromIntegral l + r))
numericAdd (Double l) (Integer r) = Right (Double (l + fromIntegral r))
numericAdd _ _ = Left $ mkEvalError Nothing "numeric operation requires Int or Double"

numericSub :: Value -> Value -> Either EvalError Value
numericSub (Integer l) (Integer r) = Right (Integer (l - r))
numericSub (Double l) (Double r) = Right (Double (l - r))
numericSub (Integer l) (Double r) = Right (Double (fromIntegral l - r))
numericSub (Double l) (Integer r) = Right (Double (l - fromIntegral r))
numericSub _ _ = Left $ mkEvalError Nothing "numeric operation requires Int or Double"

numericMul :: Value -> Value -> Either EvalError Value
numericMul (Integer l) (Integer r) = Right (Integer (l * r))
numericMul (Double l) (Double r) = Right (Double (l * r))
numericMul (Integer l) (Double r) = Right (Double (fromIntegral l * r))
numericMul (Double l) (Integer r) = Right (Double (l * fromIntegral r))
numericMul _ _ = Left $ mkEvalError Nothing "numeric operation requires Int or Double"

numericDiv :: Value -> Value -> Either EvalError Value
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
numericDiv _ _ = Left $ mkEvalError Nothing "numeric operation requires Int or Double"

numericDivLeft :: Value -> Value -> Either EvalError Value
numericDivLeft = flip numericDiv

data IndexKey = IdxInt Int | IdxKey String deriving (Eq, Show)

setAt :: [IndexKey] -> Value -> Value -> Either EvalError Value
setAt [] _ _ = Left $ mkEvalError Nothing "invalid assignment target"
setAt (IdxInt i:is) newVal (List vals len)
    | i < 0 || i >= len = Left $ mkEvalError Nothing ("index " ++ show i ++ " out of bounds (length " ++ show len ++ ")")
    | null is =
        let old = vals !! i
        in if getValueType old == getValueType newVal
              then Right (List (replaceAt i newVal vals) len)
              else Left $ mkEvalError Nothing ("type mismatch: cannot assign " ++ getValueType newVal ++ " to " ++ getValueType old)
    | otherwise = do
        nested <- case vals !! i of
                    l@(List _ _) -> Right l
                    o@(Object _) -> Right o
                    v            -> Left $ mkEvalError Nothing ("cannot index into " ++ getValueType v)
        updatedNested <- setAt is newVal nested
        Right (List (replaceAt i updatedNested vals) len)
setAt (IdxKey k:is) newVal (Object kvs)
    | null is = Right (Object (upsert k newVal kvs))
    | otherwise = do
        nested <- case lookup k kvs of
                    Just v@(Object _) -> Right v
                    Just v@(List _ _) -> Right v
                    Just v            -> Left $ mkEvalError Nothing ("cannot index into " ++ getValueType v)
                    Nothing           -> Left $ mkEvalError Nothing ("key '" ++ k ++ "' not found")
        updatedNested <- setAt is newVal nested
        Right (Object (upsert k updatedNested kvs))
setAt (IdxInt _ : _) _ (Object _) = Left $ mkEvalError Nothing "expected String key for Object, found Int"
setAt (IdxKey _ : _) _ (List _ _) = Left $ mkEvalError Nothing "expected Int index for List, found String"
setAt _ _ v = Left $ mkEvalError Nothing ("cannot assign into " ++ getValueType v)

upsert :: String -> Value -> [(String, Value)] -> [(String, Value)]
upsert key val [] = [(key, val)]
upsert key val ((k,v):rest)
    | key == k  = (key, val) : rest
    | otherwise = (k,v) : upsert key val rest

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 newVal (_:xs) = newVal : xs
replaceAt n newVal (x:xs) = x : replaceAt (n-1) newVal xs

evalComparison :: BinaryOperation -> Value -> Value -> Either EvalError Value
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

evalComparison Different (Integer l) (Integer r) = Right (Boolean (l /= r))
evalComparison Different (Double l) (Double r) = Right (Boolean (l /= r))
evalComparison Different (Boolean l) (Boolean r) = Right (Boolean (l /= r))
evalComparison Different v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 /= s2))
evalComparison Different (Character l) (Character r) = Right (Boolean (l /= r))
evalComparison Different (List l _) (List r _) = Right (Boolean (l /= r))
evalComparison Different (Set l _) (Set r _) = Right (Boolean (l /= r))

evalComparison And (Boolean l) (Boolean r) = Right (Boolean (l && r))
evalComparison And l r = Left $ mkEvalError Nothing ("logical AND requires Bool, found " ++ getValueType l ++ " and " ++ getValueType r)

evalComparison Or (Boolean l) (Boolean r) = Right (Boolean (l || r))
evalComparison Or l r = Left $ mkEvalError Nothing ("logical OR requires Bool, found " ++ getValueType l ++ " and " ++ getValueType r)

evalComparison GreaterThan (Integer l) (Integer r) = Right (Boolean (l > r))
evalComparison GreaterThan (Double l) (Double r) = Right (Boolean (l > r))
evalComparison GreaterThan v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 > s2))
evalComparison GreaterThan (List l ls) (List r rs) = Right (Boolean (ls > rs))
evalComparison GreaterThan (Set l ls) (Set r rs) = Right (Boolean (ls > rs))

evalComparison LessThan (Integer l) (Integer r) = Right (Boolean (l < r))
evalComparison LessThan (Double l) (Double r) = Right (Boolean (l < r))
evalComparison LessThan v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 < s2))
evalComparison LessThan (List l ls) (List r rs) = Right (Boolean (ls < rs))
evalComparison LessThan (Set l ls) (Set r rs) = Right (Boolean (ls < rs))

evalComparison GreaterThanEq (Integer l) (Integer r) = Right (Boolean (l >= r))
evalComparison GreaterThanEq (Double l) (Double r) = Right (Boolean (l >= r))
evalComparison GreaterThanEq v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 >= s2))
evalComparison GreaterThanEq (List l ls) (List r rs) = Right (Boolean (ls >= rs))
evalComparison GreaterThanEq (Set l ls) (Set r rs) = Right (Boolean (ls >= rs))

evalComparison LessThanEq (Integer l) (Integer r) = Right (Boolean (l <= r))
evalComparison LessThanEq (Double l) (Double r) = Right (Boolean (l <= r))
evalComparison LessThanEq v1 v2
    | Just s1 <- valueToString v1
    , Just s2 <- valueToString v2 = Right (Boolean (s1 <= s2))
evalComparison LessThanEq (List l ls) (List r rs) = Right (Boolean (ls <= rs))
evalComparison LessThanEq (Set l ls) (Set r rs) = Right (Boolean (ls <= rs))

evalComparison op l r = Left $ mkEvalError Nothing ("cannot apply " ++ show op ++ " to " ++ getValueType l ++ " and " ++ getValueType r)

evalUnaryOp :: UnaryOperation -> Value -> Either EvalError Value
evalUnaryOp Negate (Integer n) = Right (Integer (-n))
evalUnaryOp Negate (Double d) = Right (Double (-d))
evalUnaryOp Negate v = Left $ mkEvalError Nothing ("cannot negate " ++ getValueType v)
evalUnaryOp Not (Boolean b) = Right (Boolean (not b))
evalUnaryOp Not v = Left $ mkEvalError Nothing ("logical NOT requires Bool, found " ++ getValueType v)
evalUnaryOp TypeOf v =
    let t = getValueType v
    in Right (List (map Character t) (length t))
evalUnaryOp SizeOf v
    | Just s <- valueToString v = Right (Integer (length s))
evalUnaryOp SizeOf (List _ len) = Right (Integer len)
evalUnaryOp SizeOf (Set _ len) = Right (Integer len)
evalUnaryOp SizeOf (Object kvs) = Right (Integer (length kvs))
evalUnaryOp SizeOf v = Left $ mkEvalError Nothing ("cannot get size of " ++ getValueType v)

evalUnion :: BinaryOperation -> Value -> Value -> Either EvalError Value
evalUnion Concatenation (List l1 len1) (List l2 len2)
    | allCharacter l1 && allCharacter l2 =
        let combined = l1 ++ l2
        in Right (List combined (len1 + len2))
    | allSameType (l1 ++ l2) = Right (List (l1 ++ l2) (len1 + len2))
    | otherwise = Left $ mkEvalError Nothing "list concatenation requires same element types"
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

evalUnion op l r = Left $ mkEvalError Nothing ("cannot apply " ++ show op ++ " to " ++ getValueType l ++ " and " ++ getValueType r)

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
prettyValue (_) = "<value>"

intercalateWith :: String -> [String] -> String
intercalateWith _ [] = ""
intercalateWith _ [x] = x
intercalateWith sep (x:xs) = x ++ sep ++ intercalateWith sep xs
