
module Data.WODD.Types where


---- IMPORTS ---------------------------------------
import Prelude

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified Data.Vector.Unboxed as U

import qualified Data.HashMap.Strict as Map

import Data.Function (on)
import Data.Hashable (Hashable)

----------------------------------------------------


data Node v = Terminal -- ^ a value of type d 
			 | Branch !v -- ^ a node variable
			    	  (U.Vector Arc) -- ^ vector of arcs from this node
	deriving Show

data Order v = Order {comp :: v -> v -> Ordering}
fromList :: (Eq a, Hashable a) => [a] -> Order a
fromList xs = let m = Map.fromList $ zip xs [0..] 
			  in Order (compare `on` (m Map.!))

type Index = Int

type Value = Int -- ^ values in variable domain are indexed by integers
data Domain = Domain {cardinality :: Int} deriving Show

type Arc = (Value, 
			Double, 
			Index) 

data WODD v = WODD 
	{ core :: !(IntMap (Node v)),
	  next :: !Index,
	  top  :: !Index
	  --cache :: ! (IntIntMap Index)
	}


