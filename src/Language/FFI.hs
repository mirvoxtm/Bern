{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Language.FFI where

import Foreign
import Foreign.C.Types (CInt(..), CDouble(..), CChar(..))
import Foreign.C.String (CString, newCString, peekCString, withCString, castCharToCChar, castCCharToChar)
import Foreign.Ptr
import Language.Helpers
import Language.Ast
import Control.Exception (try, SomeException, catch)
import System.IO (hPutStrLn, stderr)
import System.Posix.DynamicLinker (dlopen, dlsym, dlclose, RTLDFlags(..))
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Supported Bern types for FFI
data BernCType
    = BernInt
    | BernDouble
    | BernBool
    | BernChar
    | BernString
    | BernVoid
    deriving (Show, Eq)

-- Parse type string to BernCType
parseType :: String -> Maybe BernCType
parseType "int"    = Just BernInt
parseType "double" = Just BernDouble
parseType "bool"   = Just BernBool
parseType "char"   = Just BernChar
parseType "string" = Just BernString
parseType "void"   = Just BernVoid
parseType _        = Nothing

-- Convert a Bern Value to a string representation
valueToString' :: Value -> Maybe String
valueToString' (Integer n)     = Just (show n)
valueToString' (Double d)      = Just (show d)
valueToString' (Boolean True)  = Just "true"
valueToString' (Boolean False) = Just "false"
valueToString' (Character c)   = Just [c]
valueToString' v               = valueToString v

-- Get the return type of a function
getReturnType :: String -> Maybe BernCType
getReturnType = parseType

-- Convert Bern value to C argument
valueToCArg :: BernCType -> Value -> IO (Maybe (Ptr ()))
valueToCArg BernInt (Integer n) = do
    ptr <- malloc :: IO (Ptr CInt)
    poke ptr (fromIntegral n :: CInt)
    return $ Just (castPtr ptr)
valueToCArg BernDouble (Double d) = do
    ptr <- malloc :: IO (Ptr CDouble)
    poke ptr (realToFrac d :: CDouble)
    return $ Just (castPtr ptr)
valueToCArg BernBool (Boolean b) = do
    ptr <- malloc :: IO (Ptr CInt)
    poke ptr (if b then 1 else 0 :: CInt)
    return $ Just (castPtr ptr)
valueToCArg BernChar (Character c) = do
    ptr <- malloc :: IO (Ptr CChar)
    poke ptr (castCharToCChar c)
    return $ Just (castPtr ptr)
valueToCArg BernString val = do
    case valueToString val of
        Just s -> do
            cstr <- newCString s
            return $ Just (castPtr cstr)
        Nothing -> return Nothing
valueToCArg _ _ = return Nothing

-- Convert C result to Bern value
cResultToValue :: BernCType -> Ptr () -> IO Value
cResultToValue BernInt ptr = do
    val <- peek (castPtr ptr :: Ptr CInt)
    return $ Integer (fromIntegral val)
cResultToValue BernDouble ptr = do
    val <- peek (castPtr ptr :: Ptr CDouble)
    return $ Double (realToFrac val)
cResultToValue BernBool ptr = do
    val <- peek (castPtr ptr :: Ptr CInt)
    return $ Boolean (val /= 0)
cResultToValue BernChar ptr = do
    val <- peek (castPtr ptr :: Ptr CChar)
    return $ Character (castCCharToChar val)
cResultToValue BernString ptr = do
    let cstr = castPtr ptr :: CString
    str <- peekCString cstr
    return $ List (map Character str) (length str)

-- Foreign imports for dynamic function calling
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

-- Load a C function from a shared library
loadCFunction
    :: String
    -> String
    -> [String]
    -> String
    -> IO (Either String ([Value] -> IO Value))
loadCFunction libPath funcName argTypeStrings retTypeStr = do
    let argTypes = map parseType argTypeStrings
    
    if any (== Nothing) argTypes
        then return $ Left ("Invalid argument type in FFI declaration for " ++ funcName)
        else case getReturnType retTypeStr of
            Nothing ->
                return $ Left ("Invalid return type '" ++ retTypeStr ++ "' in FFI declaration for " ++ funcName)
            Just retType -> do
                result <- try $ loadAndCall libPath funcName (map (\(Just t) -> t) argTypes) retType
                case result of
                    Left (e :: SomeException) ->
                        return $ Left ("Failed to load function " ++ funcName ++ ": " ++ show e)
                    Right func ->
                        return $ Right func

-- Load the dynamic library and get the function pointer
loadAndCall :: String -> String -> [BernCType] -> BernCType -> IO ([Value] -> IO Value)
loadAndCall libPath funcName argTypes retType = do
    -- Open the dynamic library with RTLD_LAZY to handle dependencies better
    lib <- catch (dlopen libPath [RTLD_LAZY, RTLD_GLOBAL])
                 (\(e :: SomeException) -> do
                     hPutStrLn stderr $ "Failed to open library " ++ libPath ++ ": " ++ show e
                     error $ "Failed to open library: " ++ show e)
    
    -- Get function pointer (dlsym returns FunPtr, we cast to Ptr ())
    funPtrRaw <- catch (dlsym lib funcName)
                       (\(e :: SomeException) -> do
                           hPutStrLn stderr $ "Failed to find symbol " ++ funcName ++ " in " ++ libPath ++ ": " ++ show e
                           error $ "Failed to find symbol: " ++ show e)
    let funPtr = castFunPtrToPtr funPtrRaw
    
    -- Return a wrapper function
    return $ \args -> do
        if length args /= length argTypes
            then return Undefined
            else callCFunction funPtr argTypes retType args
-- Call the C function with given arguments
callCFunction :: Ptr () -> [BernCType] -> BernCType -> [Value] -> IO Value
callCFunction funPtr argTypes retType args = do
    result <- try $ callCFunctionUnsafe funPtr argTypes retType args
    case result of
        Left (e :: SomeException) -> do
            hPutStrLn stderr $ "FFI call error: " ++ show e
            return Undefined
        Right val -> return val

-- Unsafe call implementation (handles different signatures)
callCFunctionUnsafe :: Ptr () -> [BernCType] -> BernCType -> [Value] -> IO Value

-- Void return, no args
callCFunctionUnsafe funPtr [] BernVoid _ = do
    let f = mkVoidFun (castPtrToFunPtr funPtr)
    f
    return Undefined

-- Void return, one int arg
callCFunctionUnsafe funPtr [BernInt] BernVoid [arg] = do
    let f = mkIntVoidFun (castPtrToFunPtr funPtr)
    case arg of
        Integer n -> do
            f (fromIntegral n)
            return Undefined
        _ -> return Undefined

-- Void return, three int args and one string (InitWindow)
callCFunctionUnsafe funPtr [BernInt, BernInt, BernString] BernVoid [arg1, arg2, arg3] = do
    let f = mkIntIntStringVoidFun (castPtrToFunPtr funPtr)
    case (arg1, arg2, valueToString arg3) of
        (Integer a, Integer b, Just s) -> withCString s $ \cstr -> do
            f (fromIntegral a) (fromIntegral b) cstr
            return Undefined
        _ -> return Undefined

-- Void return, five args: string, int, int, int, int (DrawText)
callCFunctionUnsafe funPtr [BernString, BernInt, BernInt, BernInt, BernInt] BernVoid [arg1, arg2, arg3, arg4, arg5] = do
    let f = mkStringIntIntIntIntVoidFun (castPtrToFunPtr funPtr)
    case (valueToString arg1, arg2, arg3, arg4, arg5) of
        (Just s, Integer x, Integer y, Integer size, Integer color) -> withCString s $ \cstr -> do
            f cstr (fromIntegral x) (fromIntegral y) (fromIntegral size) (fromIntegral color)
            return Undefined
        _ -> return Undefined

-- Int return, no args
callCFunctionUnsafe funPtr [] BernInt _ = do
    let f = mkIntFun (castPtrToFunPtr funPtr)
    result <- f
    return $ Integer (fromIntegral result)

-- Int return, one int arg
callCFunctionUnsafe funPtr [BernInt] BernInt [arg] = do
    let f = mkIntIntFun (castPtrToFunPtr funPtr)
    case arg of
        Integer n -> do
            result <- f (fromIntegral n)
            return $ Integer (fromIntegral result)
        _ -> return Undefined

-- Int return, two int args
callCFunctionUnsafe funPtr [BernInt, BernInt] BernInt [arg1, arg2] = do
    let f = mkIntIntIntFun (castPtrToFunPtr funPtr)
    case (arg1, arg2) of
        (Integer a, Integer b) -> do
            result <- f (fromIntegral a) (fromIntegral b)
            return $ Integer (fromIntegral result)
        _ -> return Undefined

-- Double return, no args
callCFunctionUnsafe funPtr [] BernDouble _ = do
    let f = mkDoubleFun (castPtrToFunPtr funPtr)
    result <- f
    return $ Double (realToFrac result)

-- Double return, one double arg
callCFunctionUnsafe funPtr [BernDouble] BernDouble [arg] = do
    let f = mkDoubleDoubleFun (castPtrToFunPtr funPtr)
    case arg of
        Double d -> do
            result <- f (realToFrac d)
            return $ Double (realToFrac result)
        _ -> return Undefined

-- Double return, two double args
callCFunctionUnsafe funPtr [BernDouble, BernDouble] BernDouble [arg1, arg2] = do
    let f = mkDoubleDoubleDoubleFun (castPtrToFunPtr funPtr)
    case (arg1, arg2) of
        (Double a, Double b) -> do
            result <- f (realToFrac a) (realToFrac b)
            return $ Double (realToFrac result)
        _ -> return Undefined

-- Int return, one string arg
callCFunctionUnsafe funPtr [BernString] BernInt [arg] = do
    let f = mkStringIntFun (castPtrToFunPtr funPtr)
    case valueToString arg of
        Just s -> withCString s $ \cstr -> do
            result <- f cstr
            return $ Integer (fromIntegral result)
        Nothing -> return Undefined

-- String return, one string arg
callCFunctionUnsafe funPtr [BernString] BernString [arg] = do
    let f = mkStringStringFun (castPtrToFunPtr funPtr)
    case valueToString arg of
        Just s -> withCString s $ \cstr -> do
            resultPtr <- f cstr
            resultStr <- peekCString resultPtr
            return $ List (map Character resultStr) (length resultStr)
        Nothing -> return Undefined

-- Fallback for unsupported signatures
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
showValue _             = "<? >"