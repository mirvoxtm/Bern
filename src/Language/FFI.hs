{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Language.FFI where

import Foreign
import Foreign.C.Types    (CInt(..), CDouble(..), CChar(..))
import Foreign.C.String   (CString, newCString, peekCString, withCString,
                           castCharToCChar, castCCharToChar)
import Foreign.Ptr
import Control.Exception  (try, SomeException, catch)
import System.IO          (hPutStrLn, stderr)

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
    deriving (Show, Eq)

parseType :: String -> Maybe BernCType
parseType "int"    = Just BernInt
parseType "double" = Just BernDouble
parseType "bool"   = Just BernBool
parseType "char"   = Just BernChar
parseType "string" = Just BernString
parseType "void"   = Just BernVoid
parseType _        = Nothing

getReturnType :: String -> Maybe BernCType
getReturnType = parseType

-- | Platform-independent library handle
data LibHandle
#ifdef mingw32_HOST_OS
    = WinLib HMODULE
#else
    = PosixLib Posix.DL
#endif

-- | Load shared library (cross-platform)
loadLib :: FilePath -> IO LibHandle
#ifdef mingw32_HOST_OS
loadLib path = fmap WinLib (loadLibrary path)
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

-- | Close library handle (optional — usually omitted for long-lived programs)
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
cResultToValue BernVoid _ = return Undefined


-- Dynamic Wrappers
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
    mkDoubleFun :: FunPtr (IO CDouble) -> IO CDouble

foreign import ccall "dynamic"
    mkDoubleDoubleFun :: FunPtr (CDouble -> IO CDouble) -> CDouble -> IO CDouble

foreign import ccall "dynamic"
    mkDoubleDoubleDoubleFun :: FunPtr (CDouble -> CDouble -> IO CDouble) -> CDouble -> CDouble -> IO CDouble

foreign import ccall "dynamic"
    mkStringIntFun :: FunPtr (CString -> IO CInt) -> CString -> IO CInt

foreign import ccall "dynamic"
    mkStringStringFun :: FunPtr (CString -> IO CString) -> CString -> IO CString

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
                           -- closeLib lib   -- optional: usually keep open
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