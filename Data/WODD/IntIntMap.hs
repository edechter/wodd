
module Data.WODD.IntIntMap 
       
( IntIntMap ()       
, empty, lookup, insert, singleton
)  
       
where

import Prelude hiding ( lookup )  
  
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as M

newtype IntIntMap v = IntIntMap (IntMap (IntMap v))

empty :: IntIntMap v
empty = IntIntMap M.empty
 
singleton :: (M.Key, M.Key) -> v -> IntIntMap v
singleton (i, j) v = 
    IntIntMap $ M.singleton i (M.singleton j v) 

lookup :: (M.Key, M.Key) -> IntIntMap b -> Maybe b
lookup (i, j) (IntIntMap mm) = do
    m <- M.lookup i mm
    M.lookup j m

insert :: (M.Key, M.Key) -> v -> IntIntMap v -> IntIntMap v
insert (i, j) v (IntIntMap mm) = 
    case M.lookup i mm of
        Nothing -> IntIntMap 
                   $ M.insert i (M.singleton j v) mm 
        Just m -> IntIntMap           
                   $ M.insert i (M.insert j v m) mm 
                   