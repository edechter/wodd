{-# Language ParallelListComp #-}

module Data.WODD.Core where

import Prelude

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified Data.Vector.Unboxed as U

import Data.Set()
import qualified Data.Set as S

import Control.Arrow ( (&&&) )

import Control.Monad.State.Strict
   (State, runState, evalState, get, put, gets, modify)


type Index = Int

type Value = Int -- ^ values in variable domain are indexed by integers
data Domain = Domain {cardinality :: Int} deriving Show

type Arc = (Value, 
			Double, 
			Index) 
arcLabel :: Arc -> Value
arcLabel  (l, _, _) = l

arcWeight :: Arc -> Double
arcWeight (_, w, _) = w

arcTarget :: Arc -> Index
arcTarget (_, _, t) = t  

data Node v = Terminal -- ^ a value of type d 
			 | Branch !v -- ^ a node variable
			    	  (U.Vector Arc) -- ^ vector of arcs from this node
	deriving Show

termId = 0

data WODD v = WODD 
	{ core :: !(IntMap (Node v)),
	  next :: !Index,
	  top  :: !Index
	  --cache :: ! (IntIntMap Index)
	}

empty :: WODD v 
empty = WODD {core = IntMap.singleton 0 Terminal,
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

register :: Ord v 
		=>  Node v
		-> 	State (WODD v) Index
register node = do
		i <- fresh
		s <- get
		put $! s {core = IntMap.insert i node $ core s}
		return i

-- | toDot outputs a string in format suitable for input to the "dot" program
-- from the graphviz suite.
toDot :: (Show v) => WODD v -> String
toDot (WODD idmap _ top) | IntMap.null idmap = "digraph BDD {}" 
toDot (WODD idmap _ top) =
    unlines [ "digraph BDD {"
            -- Start in oval mode
            , "node[shape=oval];"
            , evalState (helper $ idToNode top) S.empty
            , "}"
            ]
  where
    idToNode = id &&& (idmap IntMap.!)

    mkLabel lbl = "[label=\"" ++ lbl ++ "\"];"

    helper (thisId, Terminal) = return $
        -- switch to rectangle nodes for the leaf, before going back to ovals.
        unlines [ "node[shape=rectangle];"
                , show thisId 
                , "node[shape=oval];"
                ]
    helper (thisId, Branch vid arcs) = do
        -- Ensure we don't traverse children multiple times, if we have more
        -- than one edge into a given node.
        beenHere <- gets (thisId `S.member`)
        if beenHere
            then return ""
            else do
                strs <- mapM (helper . idToNode . arcTarget) (U.toList arcs)
                modify (thisId `S.insert`)
                let idStr = show thisId
                return $ unlines $ 
                   [ idStr ++ mkLabel (show vid)] 
                   ++ concat [ [s, idStr ++ "->" ++ show i ++ mkLabel (show v) ] 
                   				| (v, w, i) <- U.toList arcs | s <- strs]


data Label = Label String deriving (Eq, Ord)
instance Show Label where
	show (Label l) = l

test :: WODD Label
test = make $ do 
	nodeX1 <- register $ Branch (Label "x") $ U.fromList [(0, 0.5, termId), (1, 0.5, termId)]
	nodeX2 <- register $ Branch (Label "x") $ U.fromList [(0, 0.9, termId), (1, 0.1, termId)]
	node <- register $ Branch (Label "y") $ U.fromList [(0, 0.3, nodeX1), (1, 0.3, nodeX2)]
	return node

