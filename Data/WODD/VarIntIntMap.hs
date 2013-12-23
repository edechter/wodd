module Data.WODD.VarIntIntMap where

import Data.IntMap (Key)
import qualified Data.WODD.IntIntMap as I
import qualified Data.Map.Strict as M
import Prelude hiding ( lookup )  
  

newtype VarIntIntMap k v = 
    VarIntIntMap ( M.Map k ( I.IntIntMap v ))
    
empty :: VarIntIntMap k v
empty = VarIntIntMap M.empty

lookup :: Ord k => (k, Key, Key) -> VarIntIntMap k b -> Maybe b
lookup (k, i, j) (VarIntIntMap mm) = do
    m <- M.lookup k mm
    I.lookup (i, j) m

insert :: Ord k => (k, Key, Key) -> v -> VarIntIntMap k v -> VarIntIntMap k v
insert (k, i, j) v (VarIntIntMap mm) = 
    case M.lookup k mm of
        Nothing -> VarIntIntMap 
                   $ M.insert k (I.singleton (i, j) v) mm
        Just m -> VarIntIntMap           
                   $ M.insert k (I.insert (i, j) v m) mm
