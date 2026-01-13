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
getValueType (Character _) = "Character"
getValueType (List vals _) | allCharacter vals = "Text"
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
getValueOnly (Character c) = [c]
getValueOnly (List vals _) | allCharacter vals = concatMap getValueOnly vals
getValueOnly (Function _) = "<function>"
getValueOnly (Lambda _) = "<lambda>"

allSameType :: [Value] -> Bool
allSameType [] = True
allSameType (x:xs) = all (\v -> getValueType v == getValueType x) xs

allCharacter :: [Value] -> Bool
allCharacter [] = True
allCharacter (x:xs) = case x of
    Character _ -> all isCharacter xs
    _           -> False
  where
    isCharacter (Character _) = True
    isCharacter _             = False

valueToString :: Value -> Maybe String
valueToString (List vals _) | allCharacter vals = Just (map (\(Character c) -> c) vals)
valueToString _ = Nothing