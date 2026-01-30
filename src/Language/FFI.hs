{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Language.FFI where

import Foreign
import Foreign.C.Types    (CInt(..), CDouble(..), CChar(..), CUChar(..), CFloat(..), CShort(..), CUShort(..))
import Foreign.C.String   (CString, newCString, peekCString, withCString,
                           castCharToCChar, castCCharToChar)
import Foreign.Ptr
import Control.Exception  (try, SomeException, catch)
import System.IO          (hPutStrLn, stderr)
import System.FilePath    (takeDirectory, normalise)
import Data.Word          (Word8, Word16, Word32, Word64)
import Data.Bits          (shiftL, (.|.))
import Data.IORef
import System.IO.Unsafe   (unsafePerformIO)
import qualified Data.Map.Strict as Map
import qualified Foreign.LibFFI as FFI

#ifdef mingw32_HOST_OS
import System.Win32.DLL      (getProcAddress, loadLibrary, freeLibrary)
import System.Win32.Types     (HMODULE, Addr, failIfNull)
import Foreign.Ptr            (Ptr, castPtrToFunPtr)
#else
import qualified System.Posix.DynamicLinker as Posix
#endif

import Language.Helpers
import Language.Ast

-- Supported Bern types for FFI (ENHANCED WITH C-SPECIFIC TYPES)
data BernCType
    = BernInt           -- 32-bit signed int
    | BernDouble        -- 64-bit double
    | BernBool          -- 8-bit bool
    | BernChar          -- 8-bit char
    | BernString        -- char*
    | BernVoid          -- void
    | BernByte          -- 8-bit unsigned (uint8_t / unsigned char)
    | BernShort         -- 16-bit signed short
    | BernUShort        -- 16-bit unsigned short
    | BernLong          -- 32-bit signed (same as Int for most platforms)
    | BernFloat         -- 32-bit float
    | BernStruct String -- Custom struct
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
getFieldSize BernDouble = 8   -- 64-bit double
getFieldSize BernBool = 1     -- 8-bit bool
getFieldSize BernChar = 1     -- 8-bit char
getFieldSize BernByte = 1     -- 8-bit unsigned char
getFieldSize BernShort = 2    -- 16-bit short
getFieldSize BernUShort = 2   -- 16-bit unsigned short
getFieldSize BernLong = 4     -- 32-bit long
getFieldSize BernFloat = 4    -- 32-bit float
getFieldSize _ = 0

-- Map Bern Type (from Ast.hs) to C type
typeToCType :: Type -> BernCType
typeToCType TInt = BernInt
typeToCType TDouble = BernDouble
typeToCType TBool = BernBool
typeToCType TChar = BernChar
typeToCType TString = BernString
typeToCType (TCustom "Byte") = BernByte
typeToCType (TCustom "UChar") = BernByte
typeToCType (TCustom "Short") = BernShort
typeToCType (TCustom "UShort") = BernUShort
typeToCType (TCustom "Float") = BernFloat
typeToCType (TCustom name) = BernStruct name
typeToCType TList = BernStruct "List"
typeToCType TSet = BernStruct "Set"

-- UNIVERSAL FIX: Infer struct layout from ADT definition (uses type annotations!)
inferStructLayoutFromDef :: String -> [Type] -> StructLayout
inferStructLayoutFromDef typeName types =
    let fields = inferFields types 0
        totalSize = if null fields then 0 else fieldOffset (last fields) + fieldSize (last fields)
    in StructLayout typeName totalSize fields
  where
    inferFields :: [Type] -> Int -> [FieldDef]
    inferFields [] _ = []
    inferFields (t:ts) offset =
        let ctype = typeToCType t
            fsize = getFieldSize ctype
            field = FieldDef ("field" ++ show (length ts)) ctype offset fsize
            nextOffset = offset + fsize
        in field : inferFields ts nextOffset

-- Fallback: Infer from runtime values (less accurate but still universal)
inferStructLayoutFromValues :: String -> [Value] -> StructLayout
inferStructLayoutFromValues typeName values =
    let fields = inferFields values 0
        totalSize = if null fields then 0 else fieldOffset (last fields) + fieldSize (last fields)
    in StructLayout typeName totalSize fields
  where
    inferFields :: [Value] -> Int -> [FieldDef]
    inferFields [] _ = []
    inferFields (v:vs) offset =
        let (ftype, fsize) = inferFieldType v
            field = FieldDef ("field" ++ show (length vs)) ftype offset fsize
            nextOffset = offset + fsize
        in field : inferFields vs nextOffset
    
    inferFieldType :: Value -> (BernCType, Int)
    inferFieldType (Integer n) 
        | n >= 0 && n <= 255 = (BernByte, 1)  -- Assume byte for small positive ints
        | otherwise = (BernInt, 4)
    inferFieldType (Double _) = (BernDouble, 8)
    inferFieldType (Boolean _) = (BernBool, 1)
    inferFieldType (Character _) = (BernChar, 1)
    inferFieldType _ = (BernInt, 4)

parseType :: String -> Maybe BernCType
parseType "int"    = Just BernInt
parseType "double" = Just BernDouble
parseType "float"  = Just BernFloat
parseType "bool"   = Just BernBool
parseType "char"   = Just BernChar
parseType "byte"   = Just BernByte
parseType "uchar"  = Just BernByte
parseType "short"  = Just BernShort
parseType "ushort" = Just BernUShort
parseType "string" = Just BernString
parseType "void"   = Just BernVoid
parseType typeName = Just (BernStruct typeName)

getReturnType :: String -> Maybe BernCType
getReturnType = parseType

-- Platform-independent library handle
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

-- Load shared library (cross-platform)
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

-- Get function pointer from symbol name (cross-platform)
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
    layout <- case maybeLayout of
        Just l -> return l
        -- Auto-infer layout if not registered
        Nothing -> do
            let inferredLayout = inferStructLayoutFromValues typeName values
            registerStruct inferredLayout
            return inferredLayout
    
    ptr <- mallocBytes (structSize layout)
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
            
            (BernByte, Integer n) -> do  -- Proper byte handling!
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr Word8) (fromIntegral n)
            
            (BernShort, Integer n) -> do  -- 16-bit short
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr CShort) (fromIntegral n)
            
            (BernUShort, Integer n) -> do  -- 16-bit unsigned
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr CUShort) (fromIntegral n)
            
            (BernFloat, Double d) -> do  -- 32-bit float
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr CFloat) (realToFrac d)
            
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
            BernByte -> do
                val <- peek (castPtr ptr :: Ptr Word8)
                return $ Integer (fromIntegral val)
            BernShort -> do
                val <- peek (castPtr ptr :: Ptr CShort)
                return $ Integer (fromIntegral val)
            BernUShort -> do
                val <- peek (castPtr ptr :: Ptr CUShort)
                return $ Integer (fromIntegral val)
            BernFloat -> do
                val <- peek (castPtr ptr :: Ptr CFloat)
                return $ Double (realToFrac val)
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

-- Helper to fill bytes in struct layout order
fillStructBytes :: Ptr Word8 -> [FieldDef] -> [Value] -> IO ()
fillStructBytes _ [] _ = return ()
fillStructBytes _ _ [] = return ()
fillStructBytes ptr (field:fields) (val:vals) = do
    let offset = fieldOffset field
    case (fieldType field, val) of
        (BernByte, Integer n) -> do
            let fieldPtr = plusPtr ptr offset
            poke fieldPtr (fromIntegral n :: Word8)
        
        (BernInt, Integer n) -> do
            let fieldPtr = plusPtr ptr offset
            poke (castPtr fieldPtr :: Ptr CInt) (fromIntegral n)
        
        (BernShort, Integer n) -> do
            let fieldPtr = plusPtr ptr offset
            poke (castPtr fieldPtr :: Ptr CShort) (fromIntegral n)
        
        (BernUShort, Integer n) -> do
            let fieldPtr = plusPtr ptr offset
            poke (castPtr fieldPtr :: Ptr CUShort) (fromIntegral n)
        
        (BernFloat, Double d) -> do
            let fieldPtr = plusPtr ptr offset
            poke (castPtr fieldPtr :: Ptr CFloat) (realToFrac d)
        
        (BernDouble, Double d) -> do
            let fieldPtr = plusPtr ptr offset
            poke (castPtr fieldPtr :: Ptr CDouble) (realToFrac d)
        
        (BernBool, Boolean b) -> do
            let fieldPtr = plusPtr ptr offset
            poke fieldPtr (if b then 1 else 0 :: Word8)
        
        (BernChar, Character c) -> do
            let fieldPtr = plusPtr ptr offset
            poke (castPtr fieldPtr :: Ptr CChar) (castCharToCChar c)
        
        _ -> return ()
    fillStructBytes ptr fields vals

-- Pack struct fields into Word32 for passing small structs (≤4 bytes) by value
-- This matches x86-64 calling convention where small structs go in registers
packStructWord32 :: StructLayout -> [Value] -> IO Word32
packStructWord32 layout values = do
    -- Create a byte buffer
    buffer <- mallocBytes 4 :: IO (Ptr Word8)
    
    -- Fill the buffer with struct fields
    fillStructBytes buffer (structFields layout) values
    
    -- Read as Word32 (matches the CPU register size)
    result <- peek (castPtr buffer :: Ptr Word32)
    free buffer
    return result

-- Pack struct fields into Word64 for passing structs (5-8 bytes) by value
packStructWord64 :: StructLayout -> [Value] -> IO Word64
packStructWord64 layout values = do
    -- Create a byte buffer
    buffer <- mallocBytes 8 :: IO (Ptr Word8)
    
    -- Fill the buffer with struct fields
    fillStructBytes buffer (structFields layout) values
    
    -- Read as Word64
    result <- peek (castPtr buffer :: Ptr Word64)
    free buffer
    return result

valueToFFIArg :: BernCType -> Value -> IO FFI.Arg
valueToFFIArg BernInt (Integer n) = 
    return $ FFI.argCInt (fromIntegral n)

valueToFFIArg BernByte (Integer n) = 
    return $ FFI.argWord8 (fromIntegral n)

-- Short types: libffi doesn't have argCShort, so we pass as Word16
valueToFFIArg BernShort (Integer n) = 
    return $ FFI.argWord16 (fromIntegral n)

valueToFFIArg BernUShort (Integer n) = 
    return $ FFI.argWord16 (fromIntegral n)

valueToFFIArg BernFloat (Double d) = 
    return $ FFI.argCFloat (realToFrac d)

valueToFFIArg BernDouble (Double d) = 
    return $ FFI.argCDouble (realToFrac d)

valueToFFIArg BernBool (Boolean b) = 
    return $ FFI.argWord8 (if b then 1 else 0)

valueToFFIArg BernChar (Character c) = 
    return $ FFI.argCChar (castCharToCChar c)

valueToFFIArg BernString val = do
    case valueToString val of
        Just s -> do
            cstr <- newCString s
            return $ FFI.argPtr cstr
        Nothing -> 
            return $ FFI.argPtr nullPtr

valueToFFIArg (BernStruct typeName) (AlgebraicDataType name values)
    | typeName == name = do
        maybeLayout <- getStructLayout typeName
        case maybeLayout of
            Just layout -> do
                let size = structSize layout
                if size <= 4 then do
                    packed <- packStructWord32 layout values
                    return $ FFI.argWord32 packed
                else if size <= 8 then do

                    packed <- packStructWord64 layout values
                    return $ FFI.argWord64 packed
                else do

                    maybeStructPtr <- adtToStruct typeName values
                    case maybeStructPtr of
                        Just structPtr -> return $ FFI.argPtr structPtr
                        Nothing -> return $ FFI.argPtr nullPtr
            Nothing -> return $ FFI.argPtr nullPtr

valueToFFIArg _ _ = return $ FFI.argPtr nullPtr

callCFunctionUniversal :: Ptr () -> [BernCType] -> BernCType -> [Value] -> IO Value
callCFunctionUniversal funPtr argTypes retType args = do
    if length args /= length argTypes
        then return Undefined
        else do

            ffiArgs <- mapM (uncurry valueToFFIArg) (zip argTypes args)
            
            -- Call based on return type
            case retType of
                BernInt -> do
                    (result :: CInt) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retCInt ffiArgs
                    return $ Integer (fromIntegral result)
                
                BernByte -> do
                    (result :: Word8) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retWord8 ffiArgs
                    return $ Integer (fromIntegral result)
                
                -- Short types: libffi doesn't have retCShort, use retWord16
                BernShort -> do
                    (result :: Word16) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retWord16 ffiArgs
                    return $ Integer (fromIntegral (fromIntegral result :: CShort))
                
                BernUShort -> do
                    (result :: Word16) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retWord16 ffiArgs
                    return $ Integer (fromIntegral result)
                
                BernFloat -> do
                    (result :: CFloat) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retCFloat ffiArgs
                    return $ Double (realToFrac result)
                
                BernDouble -> do
                    (result :: CDouble) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retCDouble ffiArgs
                    return $ Double (realToFrac result)
                
                BernBool -> do
                    (result :: Word8) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retWord8 ffiArgs
                    return $ Boolean (result /= 0)
                
                BernChar -> do
                    (result :: CChar) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retCChar ffiArgs
                    return $ Character (castCCharToChar result)
                
                BernString -> do
                    (cstrPtr :: CString) <- FFI.callFFI (castPtrToFunPtr funPtr) (FFI.retPtr FFI.retCChar) ffiArgs
                    if cstrPtr == nullPtr
                        then return $ List [] 0
                        else do
                            str <- peekCString cstrPtr
                            return $ List (map Character str) (length str)
                
                BernStruct typeName -> do
                    (structPtr :: Ptr Word8) <- FFI.callFFI (castPtrToFunPtr funPtr) (FFI.retPtr FFI.retWord8) ffiArgs
                    if structPtr == nullPtr
                        then return Undefined
                        else do
                            maybeVal <- structToAdt typeName (castPtr structPtr :: Ptr ())
                            case maybeVal of
                                Just val -> return val
                                Nothing -> return Undefined
                
                BernVoid -> do
                    FFI.callFFI (castPtrToFunPtr funPtr) FFI.retVoid ffiArgs
                    return Undefined

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
        result <- try $ callCFunctionUniversal funPtrRaw argTypes retType args
        case result of
            Left  (e :: SomeException) -> do
                hPutStrLn stderr $ "FFI call failed: " ++ show e
                return Undefined
            Right val -> return val

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