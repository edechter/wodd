
module Data.WODD.Core where


---- IMPORTS ---------------------------------------
import Prelude

import qualified Data.IntMap.Strict as IntMap

import qualified Data.Vector.Unboxed as U

import Data.Set()
import Data.Maybe (fromMaybe)

import Control.Monad.State.Strict
   (State, runState, get, put)

import Data.Hashable (Hashable, hashWithSalt)
-----------------------------------------------------

import Data.WODD.Types

-----------------------------------------------------

arcLabel :: Arc -> Value
arcLabel  (l, _, _) = l

arcWeight :: Arc -> Double
arcWeight (_, w, _) = w

arcTarget :: Arc -> Index
arcTarget (_, _, t) = t  

termId :: Index
termId = 0

arcToLeaf :: Value -> Double -> Arc 
arcToLeaf l w = (l, w, termId)

arcToNode :: Value -> Double -> Index -> Arc
arcToNode l w n = (l, w, n)

arcsFromList :: [Arc] -> U.Vector Arc
arcsFromList arcs = U.fromList arcs

empty :: WODD v 
empty = WODD {core = IntMap.empty, --IntMap.singleton 0 Terminal,
			  next = 1, 
			  top  = 0}

size :: WODD v -> Int
size = next

make :: State ( WODD v ) Index
     -> WODD v
make action = 
    let ( i, s ) = runState action empty 
    in  i `seq` s { top = i }

fresh :: State ( WODD v ) Index
-- | Get a new unused node.
fresh = do
    s <- get
    let i = next s
    put $! s { next = succ i }
    return i

register :: Node v
		-> 	State (WODD v) Index
register Terminal = return 0
register node = do
		i <- fresh
		s <- get
		put $! s {core = IntMap.insert i node $ core s}
		return i

access :: WODD v -> Node v 
access wodd | top wodd   == 0  = Terminal
access wodd = fromMaybe
  (error "Data.WODD.access: could not find <top> in <core>.")
  (IntMap.lookup (top wodd) (core wodd))


accessAt :: WODD v -> Index -> Node v
accessAt _ 0 = Terminal
accessAt wodd idx = fromMaybe
  (error "Data.WODD.access: could not find <idx> in <core>.")
  (IntMap.lookup idx (core wodd))


data Label = Label String deriving (Eq)
instance Show Label where
	show (Label l) = l

instance Hashable Label where
	hashWithSalt i (Label l) = hashWithSalt i l

test :: WODD Label
test = make $ do 
	nodeX1 <- register $ Branch (Label "x") 
						$ arcsFromList [arcToLeaf 0 0.5, arcToLeaf 1 0.5]
	nodeX2 <- register $ Branch (Label "x") $ arcsFromList [(0, 0.9, termId), (1, 0.1, termId)]
	register $ Branch (Label "y") $ arcsFromList [(0, 0.3, nodeX1), (1, 0.3, nodeX2)]
	

