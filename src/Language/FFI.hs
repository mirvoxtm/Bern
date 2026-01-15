{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Language.FFI where

import Foreign
import Foreign.C.Types    (CInt(..), CDouble(..), CChar(..), CUChar(..), CFloat(..))
import Foreign.C.String   (CString, newCString, peekCString, withCString,
                           castCharToCChar, castCCharToChar)
import Foreign.Ptr
import Control.Exception  (try, SomeException, catch)
import System.IO          (hPutStrLn, stderr)
import System.FilePath    (takeDirectory, normalise)
import Data.Word          (Word8, Word16, Word32)
import Data.IORef
import System.IO.Unsafe   (unsafePerformIO)
import qualified Data.Map.Strict as Map

#ifdef mingw32_HOST_OS
import System.Win32.DLL      (getProcAddress, loadLibrary, freeLibrary)
import System.Win32.Types     (HMODULE, Addr, failIfNull)
import Foreign.Ptr            (Ptr, castPtrToFunPtr)
#else
import qualified System.Posix.DynamicLinker as Posix
#endif

import Language.Helpers
import Language.Ast

-- Supported Bern types for FFI
data BernCType
    = BernInt
    | BernDouble
    | BernBool
    | BernChar
    | BernString
    | BernVoid
    | BernStruct String
    deriving (Show, Eq)

-- Field definition in a struct
data FieldDef = FieldDef
    { fieldName :: String
    , fieldType :: BernCType
    , fieldOffset :: Int
    , fieldSize :: Int
    } deriving (Show, Eq)

-- Struct layout definition
data StructLayout = StructLayout
    { structName :: String
    , structSize :: Int
    , structFields :: [FieldDef]
    } deriving (Show, Eq)

-- Global registry for struct layouts
{-# NOINLINE structRegistry #-}
structRegistry :: IORef (Map.Map String StructLayout)
structRegistry = unsafePerformIO $ newIORef Map.empty

-- Register a struct layout
registerStruct :: StructLayout -> IO ()
registerStruct layout = do
    modifyIORef' structRegistry $ Map.insert (structName layout) layout

-- Get a registered struct layout
getStructLayout :: String -> IO (Maybe StructLayout)
getStructLayout name = do
    registry <- readIORef structRegistry
    return $ Map.lookup name registry

-- Helper function to calculate field sizes
getFieldSize :: BernCType -> Int
getFieldSize BernInt = 4      -- 32-bit int
getFieldSize BernDouble = 8   -- 64-bit double (assuming float in C is 4 bytes)
getFieldSize BernBool = 1     -- 8-bit bool
getFieldSize BernChar = 1     -- 8-bit char
getFieldSize _ = 0

parseType :: String -> Maybe BernCType
parseType "int"    = Just BernInt
parseType "double" = Just BernDouble
parseType "float"  = Just BernDouble
parseType "bool"   = Just BernBool
parseType "char"   = Just BernChar
parseType "string" = Just BernString
parseType "void"   = Just BernVoid
parseType typeName = Just (BernStruct typeName)

getReturnType :: String -> Maybe BernCType
getReturnType = parseType

-- | Platform-independent library handle
data LibHandle
#ifdef mingw32_HOST_OS
    = WinLib HMODULE
#else
    = PosixLib Posix.DL
#endif

#ifdef mingw32_HOST_OS
-- Windows-specific: Add DLL directory to search path
foreign import ccall unsafe "windows.h SetDllDirectoryA"
    c_SetDllDirectory :: CString -> IO Bool

foreign import ccall unsafe "windows.h AddDllDirectory"
    c_AddDllDirectory :: CString -> IO (Ptr ())

setDllDirectory :: FilePath -> IO ()
setDllDirectory path = withCString path $ \cpath -> do
    _ <- c_SetDllDirectory cpath
    return ()

addDllDirectory :: FilePath -> IO ()
addDllDirectory path = withCString path $ \cpath -> do
    _ <- c_AddDllDirectory cpath
    return ()
#endif

-- | Load shared library (cross-platform)
loadLib :: FilePath -> IO LibHandle
#ifdef mingw32_HOST_OS
loadLib path = do
    let normalizedPath = normalise path
    let dllDir = takeDirectory normalizedPath
    catch (addDllDirectory dllDir) (\(_ :: SomeException) -> return ())
    catch (setDllDirectory dllDir) (\(_ :: SomeException) -> return ())
    h <- loadLibrary normalizedPath
    return (WinLib h)
#else
loadLib path = do
    h <- Posix.dlopen path [Posix.RTLD_LAZY, Posix.RTLD_GLOBAL]
    return (PosixLib h)
#endif

-- | Get function pointer from symbol name (cross-platform)
getSymbol :: LibHandle -> String -> IO (Ptr ())
#ifdef mingw32_HOST_OS
getSymbol (WinLib h) name = do
    failIfNull "getProcAddress" $
        getProcAddress h name
#else
getSymbol (PosixLib h) name = do
    fp <- Posix.dlsym h name
    return (castFunPtrToPtr fp)
#endif

-- | Close library handle
closeLib :: LibHandle -> IO ()
#ifdef mingw32_HOST_OS
closeLib (WinLib h) = freeLibrary h >> return ()
#else
closeLib (PosixLib h) = Posix.dlclose h >> return ()
#endif

valueToString' :: Value -> Maybe String
valueToString' (Integer n)     = Just (show n)
valueToString' (Double d)      = Just (show d)
valueToString' (Boolean True)  = Just "true"
valueToString' (Boolean False) = Just "false"
valueToString' (Character c)   = Just [c]
valueToString' v               = valueToString v

-- Convert Bern ADT to C struct
adtToStruct :: String -> [Value] -> IO (Maybe (Ptr ()))
adtToStruct typeName values = do
    maybeLayout <- getStructLayout typeName
    case maybeLayout of
        Nothing -> return Nothing
        Just layout -> do
            -- Allocate memory for the struct
            ptr <- mallocBytes (structSize layout)
            -- Fill in the fields
            fillFields ptr (structFields layout) values
            return (Just (castPtr ptr))
  where
    fillFields :: Ptr Word8 -> [FieldDef] -> [Value] -> IO ()
    fillFields _ [] _ = return ()
    fillFields _ _ [] = return ()
    fillFields ptr (field:fields) (val:vals) = do
        let offset = fieldOffset field
        case (fieldType field, val) of
            (BernInt, Integer n) -> do
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr CInt) (fromIntegral n)
            (BernDouble, Double d) -> do
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr CDouble) (realToFrac d)
            (BernBool, Boolean b) -> do
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr Word8) (if b then 1 else 0)
            (BernChar, Character c) -> do
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr CChar) (castCharToCChar c)
            _ -> return ()
        fillFields ptr fields vals

-- Convert C struct back to Bern ADT
structToAdt :: String -> Ptr () -> IO (Maybe Value)
structToAdt typeName ptr = do
    maybeLayout <- getStructLayout typeName
    case maybeLayout of
        Nothing -> return Nothing
        Just layout -> do
            values <- mapM (readField (castPtr ptr)) (structFields layout)
            return $ Just (AlgebraicDataType typeName values)
  where
    readField :: Ptr Word8 -> FieldDef -> IO Value
    readField basePtr field = do
        let ptr = plusPtr basePtr (fieldOffset field)
        case fieldType field of
            BernInt -> do
                val <- peek (castPtr ptr :: Ptr CInt)
                return $ Integer (fromIntegral val)
            BernDouble -> do
                val <- peek (castPtr ptr :: Ptr CDouble)
                return $ Double (realToFrac val)
            BernBool -> do
                val <- peek (castPtr ptr :: Ptr Word8)
                return $ Boolean (val /= 0)
            BernChar -> do
                val <- peek (castPtr ptr :: Ptr CChar)
                return $ Character (castCCharToChar val)
            _ -> return Undefined

-- Convert Bern Value to C argument
valueToCArg :: BernCType -> Value -> IO (Maybe (Ptr ()))
valueToCArg BernInt (Integer n) = do
    ptr <- malloc
    poke ptr (fromIntegral n :: CInt)
    return $ Just (castPtr ptr)
valueToCArg BernDouble (Double d) = do
    ptr <- malloc
    poke ptr (realToFrac d :: CDouble)
    return $ Just (castPtr ptr)
valueToCArg BernBool (Boolean b) = do
    ptr <- malloc
    poke ptr (if b then 1 else 0 :: CInt)
    return $ Just (castPtr ptr)
valueToCArg BernChar (Character c) = do
    ptr <- malloc
    poke ptr (castCharToCChar c)
    return $ Just (castPtr ptr)
valueToCArg BernString val = do
    case valueToString val of
        Just s  -> fmap (Just . castPtr) (newCString s)
        Nothing -> return Nothing
valueToCArg (BernStruct typeName) (AlgebraicDataType name values) 
    | typeName == name = adtToStruct typeName values
valueToCArg _ _ = return Nothing

cResultToValue :: BernCType -> Ptr () -> IO Value
cResultToValue BernInt ptr = do
    v <- peek (castPtr ptr :: Ptr CInt)
    return $ Integer (fromIntegral v)
cResultToValue BernDouble ptr = do
    v <- peek (castPtr ptr :: Ptr CDouble)
    return $ Double (realToFrac v)
cResultToValue BernBool ptr = do
    v <- peek (castPtr ptr :: Ptr CInt)
    return $ Boolean (v /= 0)
cResultToValue BernChar ptr = do
    v <- peek (castPtr ptr :: Ptr CChar)
    return $ Character (castCCharToChar v)
cResultToValue BernString ptr = do
    str <- peekCString (castPtr ptr)
    return $ List (map Character str) (length str)
cResultToValue (BernStruct typeName) ptr = do
    maybeVal <- structToAdt typeName ptr
    case maybeVal of
        Just val -> return val
        Nothing -> return Undefined
cResultToValue BernVoid _ = return Undefined

-- Dynamic Wrappers (existing ones remain the same)
foreign import ccall "dynamic"
    mkVoidFun :: FunPtr (IO ()) -> IO ()

foreign import ccall "dynamic"
    mkIntFun :: FunPtr (IO CInt) -> IO CInt

foreign import ccall "dynamic"
    mkIntIntFun :: FunPtr (CInt -> IO CInt) -> CInt -> IO CInt

foreign import ccall "dynamic"
    mkIntIntIntFun :: FunPtr (CInt -> CInt -> IO CInt) -> CInt -> CInt -> IO CInt

foreign import ccall "dynamic"
    mkIntIntStringVoidFun :: FunPtr (CInt -> CInt -> CString -> IO ()) -> CInt -> CInt -> CString -> IO ()

foreign import ccall "dynamic"
    mkIntVoidFun :: FunPtr (CInt -> IO ()) -> CInt -> IO ()

foreign import ccall "dynamic"
    mkStringIntIntIntIntVoidFun :: FunPtr (CString -> CInt -> CInt -> CInt -> CInt -> IO ()) -> CString -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "dynamic"
    mkIntIntIntIntStructVoidFun :: FunPtr (CInt -> CInt -> Ptr () -> IO ()) -> CInt -> CInt -> Ptr () -> IO ()

foreign import ccall "dynamic"
    mkDoubleFun :: FunPtr (IO CDouble) -> IO CDouble

foreign import ccall "dynamic"
    mkDoubleDoubleFun :: FunPtr (CDouble -> IO CDouble) -> CDouble -> IO CDouble

foreign import ccall "dynamic"
    mkDoubleDoubleDoubleFun :: FunPtr (CDouble -> CDouble -> IO CDouble) -> CDouble -> CDouble -> IO CDouble

foreign import ccall "dynamic"
    mkStringIntFun :: FunPtr (CString -> IO CInt) -> CString -> IO CInt

foreign import ccall "dynamic"
    mkStringStringFun :: FunPtr (CString -> IO CString) -> CString -> IO CString

foreign import ccall "dynamic"
    mkBoolFun :: FunPtr (IO CInt) -> IO CInt

-- Cross platform function loader
loadCFunction
    :: String
    -> String
    -> [String]
    -> String
    -> IO (Either String ([Value] -> IO Value))
loadCFunction libPath funcName argTypeStrings retTypeStr = do
    let argTypesMb = map parseType argTypeStrings
    if any (== Nothing) argTypesMb
        then return $ Left $ "Invalid argument type(s) in FFI for " ++ funcName
        else case parseType retTypeStr of
            Nothing -> return $ Left $ "Invalid return type '" ++ retTypeStr ++ "' for " ++ funcName
            Just retType -> do
                result <- try $ loadAndCall libPath funcName (map (\(Just t) -> t) argTypesMb) retType
                case result of
                    Left  (e :: SomeException) -> return $ Left $ "Failed to load " ++ funcName ++ ": " ++ show e
                    Right f                     -> return $ Right f

loadAndCall :: String -> String -> [BernCType] -> BernCType -> IO ([Value] -> IO Value)
loadAndCall libPath funcName argTypes retType = do
    lib <- catch (loadLib libPath)
                 (\(e :: SomeException) -> do
                     hPutStrLn stderr $ "Cannot load library " ++ libPath ++ ": " ++ show e
                     error $ "Library load failed: " ++ show e)

    funPtrRaw <- catch (getSymbol lib funcName)
                       (\(e :: SomeException) -> do
                           hPutStrLn stderr $ "Symbol '" ++ funcName ++ "' not found in " ++ libPath ++ ": " ++ show e
                           error $ "Symbol lookup failed: " ++ show e)

    return $ \args -> do
        if length args /= length argTypes
            then return Undefined
            else callCFunction funPtrRaw argTypes retType args

callCFunction :: Ptr () -> [BernCType] -> BernCType -> [Value] -> IO Value
callCFunction funPtr argTypes retType args = do
    result <- try $ callCFunctionUnsafe funPtr argTypes retType args
    case result of
        Left  (e :: SomeException) -> do
            hPutStrLn stderr $ "FFI call failed: " ++ show e
            return Undefined
        Right val -> return val

callCFunctionUnsafe :: Ptr () -> [BernCType] -> BernCType -> [Value] -> IO Value

callCFunctionUnsafe funPtr [] BernBool _ = do
    let f = mkBoolFun (castPtrToFunPtr funPtr)
    r <- f
    return $ Boolean (r /= 0)

callCFunctionUnsafe funPtr [] BernVoid _ = do
    let f = mkVoidFun (castPtrToFunPtr funPtr)
    f
    return Undefined

callCFunctionUnsafe funPtr [BernInt] BernVoid [arg] = do
    let f = mkIntVoidFun (castPtrToFunPtr funPtr)
    case arg of
        Integer n -> f (fromIntegral n) >> return Undefined
        _         -> return Undefined

callCFunctionUnsafe funPtr [BernInt, BernInt, BernString] BernVoid [arg1, arg2, arg3] = do
    let f = mkIntIntStringVoidFun (castPtrToFunPtr funPtr)
    case (arg1, arg2, valueToString' arg3) of
        (Integer a, Integer b, Just s) ->
            withCString s $ \cstr -> f (fromIntegral a) (fromIntegral b) cstr >> return Undefined
        _ -> return Undefined

-- Handle struct arguments
callCFunctionUnsafe funPtr [BernInt, BernInt, BernStruct typeName] BernVoid [a1, a2, a3] = do
    let f = mkIntIntIntIntStructVoidFun (castPtrToFunPtr funPtr)
    case (a1, a2, a3) of
        (Integer x, Integer y, adt@(AlgebraicDataType name vals)) | name == typeName -> do
            maybeStructPtr <- adtToStruct typeName vals
            case maybeStructPtr of
                Just structPtr -> f (fromIntegral x) (fromIntegral y) structPtr >> return Undefined
                Nothing -> return Undefined
        _ -> return Undefined

callCFunctionUnsafe funPtr [BernString, BernInt, BernInt, BernInt, BernInt] BernVoid [a1,a2,a3,a4,a5] = do
    let f = mkStringIntIntIntIntVoidFun (castPtrToFunPtr funPtr)
    case (valueToString' a1, a2, a3, a4, a5) of
        (Just s, Integer x, Integer y, Integer sz, Integer col) ->
            withCString s $ \cstr ->
                f cstr (fromIntegral x) (fromIntegral y) (fromIntegral sz) (fromIntegral col)
                >> return Undefined
        _ -> return Undefined

callCFunctionUnsafe funPtr [] BernInt _ = do
    let f = mkIntFun (castPtrToFunPtr funPtr)
    r <- f
    return $ Integer (fromIntegral r)

callCFunctionUnsafe funPtr [BernInt] BernInt [arg] = do
    let f = mkIntIntFun (castPtrToFunPtr funPtr)
    case arg of
        Integer n -> do r <- f (fromIntegral n); return $ Integer (fromIntegral r)
        _         -> return Undefined

callCFunctionUnsafe funPtr [BernInt, BernInt] BernInt [a1, a2] = do
    let f = mkIntIntIntFun (castPtrToFunPtr funPtr)
    case (a1, a2) of
        (Integer x, Integer y) -> do r <- f (fromIntegral x) (fromIntegral y); return $ Integer (fromIntegral r)
        _                      -> return Undefined

callCFunctionUnsafe funPtr [] BernDouble _ = do
    let f = mkDoubleFun (castPtrToFunPtr funPtr)
    r <- f
    return $ Double (realToFrac r)

callCFunctionUnsafe funPtr [BernDouble] BernDouble [arg] = do
    let f = mkDoubleDoubleFun (castPtrToFunPtr funPtr)
    case arg of
        Double d -> do r <- f (realToFrac d); return $ Double (realToFrac r)
        _        -> return Undefined

callCFunctionUnsafe funPtr [BernDouble, BernDouble] BernDouble [a1, a2] = do
    let f = mkDoubleDoubleDoubleFun (castPtrToFunPtr funPtr)
    case (a1, a2) of
        (Double x, Double y) -> do r <- f (realToFrac x) (realToFrac y); return $ Double (realToFrac r)
        _                    -> return Undefined

callCFunctionUnsafe funPtr [BernString] BernInt [arg] = do
    let f = mkStringIntFun (castPtrToFunPtr funPtr)
    case valueToString' arg of
        Just s -> withCString s $ \cstr -> do r <- f cstr; return $ Integer (fromIntegral r)
        _      -> return Undefined

callCFunctionUnsafe funPtr [BernString] BernString [arg] = do
    let f = mkStringStringFun (castPtrToFunPtr funPtr)
    case valueToString' arg of
        Just s -> withCString s $ \cstr -> do
            ptr <- f cstr
            str <- peekCString ptr
            return $ List (map Character str) (length str)
        _      -> return Undefined

callCFunctionUnsafe _ _ _ _ = return Undefined

showValue :: Value -> String
showValue (Integer n)   = "Integer(" ++ show n ++ ")"
showValue (Double d)    = "Double(" ++ show d ++ ")"
showValue (Boolean b)   = "Boolean(" ++ show b ++ ")"
showValue (Character c) = "Char('" ++ [c] ++ "')"
showValue v
    | Just s <- valueToString v = "String(\"" ++ s ++ "\")"
showValue (List _ len)  = "List[" ++ show len ++ "]"
showValue (Set _ len)   = "Set{" ++ show len ++ "}"
showValue _             = "<?>"