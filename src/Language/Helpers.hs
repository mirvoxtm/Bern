module Language.Helpers where
import Language.Ast

getValueType :: Value -> String
getValueType (Integer _) = "Integer"
getValueType (Double _) = "Double"
getValueType NaN = "NaN"
getValueType Undefined = "Undefined"
getValueType (Boolean _) = "Boolean"
getValueType (Set _ _) = "Set"
getValueType (List _ _) = "List"
getValueType (Object _) = "Object"
getValueType (TextLiteral _ _) = "TextLiteral"
getValueType (Character _) = "Character"
getValueType (Text _ _) = "Text"
getValueType (Function _) = "Function"
getValueType (Lambda _) = "Lambda"
getValueType (AlgebraicDataType name _) = name

getValueOnly :: Value -> String
getValueOnly (Integer n) = show n
getValueOnly (Double d) = show d
getValueOnly NaN = "NaN"
getValueOnly Undefined = "Undefined"
getValueOnly (Boolean b) = show b
getValueOnly (Set s _) = show s
getValueOnly (List l _) = show l
getValueOnly (Object o) = show o
getValueOnly (TextLiteral t _) = t
getValueOnly (Character c) = [c]
getValueOnly (Text t _) = t
getValueOnly (Function _) = "<function>"
getValueOnly (Lambda _) = "<lambda>"

allSameType :: [Value] -> Bool
allSameType [] = True
allSameType (x:xs) = all (\v -> getValueType v == getValueType x) xs