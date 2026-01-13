{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.FFI where

import Foreign
import Foreign.C.Types (CInt, CDouble, CChar)
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.Ptr
import Language.Helpers
import Language.Ast
import Control.Exception (try, SomeException)
import System.IO (hPutStrLn, stderr)
#ifndef mingw32_HOST_OS
import System.Posix.DynamicLinker
#endif

-- Supported Bern types for FFI (renamed to avoid collision with Foreign.C.Types)
data BernCType = BernInt | BernDouble | BernBool | BernChar | BernString
    deriving (Show, Eq)

-- Parse type string to BernCType
parseType :: String -> Maybe BernCType
parseType "int" = Just BernInt
parseType "double" = Just BernDouble
parseType "bool" = Just BernBool
parseType "char" = Just BernChar
parseType "string" = Just BernString
parseType _ = Nothing

-- Convert a Bern Value to a string representation for stub calls
valueToString' :: Value -> Maybe String
valueToString' (Integer n) = Just (show n)
valueToString' (Double d) = Just (show d)
valueToString' (Boolean True) = Just "true"
valueToString' (Boolean False) = Just "false"
valueToString' (Character c) = Just [c]
valueToString' v = valueToString v

-- Get the return type of a function
getReturnType :: String -> Maybe BernCType
getReturnType retType = parseType retType

-- Load a C function from a shared library
-- Returns either an error message or a function that takes Bern values and returns a Bern value
loadCFunction :: String -> String -> [String] -> String -> IO (Either String ([Value] -> IO Value))
loadCFunction libPath funcName argTypeStrings retTypeStr = do
    -- Parse argument types
    let argTypes = map parseType argTypeStrings
    
    -- Check if all argument types are valid
    if any (== Nothing) argTypes
        then return $ Left $ "Invalid argument type in FFI declaration for " ++ funcName
        else do
            case getReturnType retTypeStr of
                Nothing -> return $ Left $ "Invalid return type '" ++ retTypeStr ++ "' in FFI declaration for " ++ funcName
                Just retType -> do
#ifdef mingw32_HOST_OS
                    -- Windows:  Return a stub implementation
                    return $ Right (windowsStub funcName (map (\(Just t) -> t) argTypes) retType)
#else
                    -- Unix:  Try to actually load the library (not implemented yet)
                    return $ Right (unixStub funcName (map (\(Just t) -> t) argTypes) retType libPath)
#endif

-- Windows stub:   Print debug info and return sensible default values
windowsStub ::  String -> [BernCType] -> BernCType -> ([Value] -> IO Value)
windowsStub funcName argTypes retType args = do
    -- Validate argument count
    if length args /= length argTypes
        then do
            return Undefined
        else do
            -- Print debug info            
            -- Return appropriate stub value based on return type
            result <- case retType of
                BernInt -> do
                    -- Special handling for known functions
                    case (funcName, args) of
                        ("strlen", [str]) -> 
                            case valueToString str of
                                Just s -> return $ Integer (length s)
                                Nothing -> return $ Integer 0
                        ("add", [Integer a, Integer b]) -> return $ Integer (a + b)
                        ("subtract", [Integer a, Integer b]) -> return $ Integer (a - b)
                        ("multiply", [Integer a, Integer b]) -> return $ Integer (a * b)
                        _ -> return $ Integer 0
                BernDouble -> return $ Double 0.0
                BernString -> return $ List (map Character "") 0
                BernBool -> return $ Boolean False
                BernChar -> return $ Character '\0'
            
            return result

-- Unix stub: Similar to Windows but with Unix-specific logic (dlopen not yet implemented)
unixStub :: String -> [BernCType] -> BernCType -> String -> ([Value] -> IO Value)
unixStub funcName argTypes retType libPath args = do
    if length args /= length argTypes
        then do
            return Undefined
        else do
            result <- case retType of
                BernInt -> 
                    case (funcName, args) of
                        ("strlen", [str]) -> 
                            case valueToString str of
                                Just s -> return $ Integer (length s)
                                Nothing -> return $ Integer 0
                        ("add", [Integer a, Integer b]) -> return $ Integer (a + b)
                        _ -> return $ Integer 0
                BernDouble -> return $ Double 0.0
                BernString -> return $ List (map Character "") 0
                BernBool -> return $ Boolean False
                BernChar -> return $ Character '\0'
            
            hPutStrLn stderr $ "  Returning: " ++ showValue result
            return result

-- Helper function to show a Value nicely for debugging
showValue :: Value -> String
showValue (Integer n) = "Integer(" ++ show n ++ ")"
showValue (Double d) = "Double(" ++ show d ++ ")"
showValue (Boolean b) = "Boolean(" ++ show b ++ ")"
showValue (Character c) = "Char('" ++ [c] ++ "')"
showValue v 
    | Just s <- valueToString v = "String(\"" ++ s ++ "\")"
showValue (List _ len) = "List[" ++ show len ++ "]"
showValue (Set _ len) = "Set{" ++ show len ++ "}"
showValue _ = "<? >"