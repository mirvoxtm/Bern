module Data.Hashtable.Hashtable
    ( Hashtable
    , emptyHashtable
    , insertHashtable
    , lookupHashtable
    ) where

import qualified Data.Map.Strict as M

-- A minimal immutable hashtable wrapper built on top of Data.Map.Strict.
newtype Hashtable k v = Hashtable { getMap :: M.Map k v }
    deriving (Show, Eq)

-- Create an empty hashtable.
emptyHashtable :: Hashtable k v
emptyHashtable = Hashtable M.empty

-- Insert or replace a key with a value.
insertHashtable :: Ord k => Hashtable k v -> k -> v -> Hashtable k v
insertHashtable (Hashtable m) k v = Hashtable (M.insert k v m)

-- Lookup a key in the hashtable.
lookupHashtable :: Ord k => Hashtable k v -> k -> Maybe v
lookupHashtable (Hashtable m) k = M.lookup k m
