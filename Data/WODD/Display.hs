{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Data.WODD.Display where

--- Imports -----------------------------------------
import Prelude
import System.IO.Temp 
import System.FilePath()
import System.IO (hClose)
--import qualified System.IO.Strict as StrictIO
import System.Exit
import System.Cmd (rawSystem)
import Control.Arrow ( (&&&) )

import Data.IntMap.Strict()
import qualified Data.IntMap.Strict as IntMap

import qualified Data.Vector.Unboxed as U

import Data.Set()
import qualified Data.Set as S
import qualified Data.Foldable as Foldable (toList)

import Control.Monad.State.Strict
   (State, runState, evalState, gets, modify)

-----------------------------------------------------

import Data.WODD.Core
import Data.WODD.Types
import Data.WODD.Apply()

class ToDot v where 
    toDot :: v -> String 

instance Show v => ToDot (WODD v) where 
    toDot = woddToDot

instance Show v => ToDot (WoddCollection v) where 
    toDot = woddCollectionToDot

-- | toDot outputs a string in format suitable for input to the "dot" program
-- from the graphviz suite.

wrapDigraph :: String -> String 
wrapDigraph s = "digraph BDD {" ++ s ++ "}" 

woddToDot :: Show v => WODD v -> String
woddToDot w = wrapDigraph $ fst $ woddToDot' 0 w

woddToDot' :: Show v => Int -> WODD v -> (String, S.Set Int)
woddToDot' _ (WODD idmap _ _) | IntMap.null idmap = ("", S.empty)
woddToDot' nodeOffset wodd@(WODD _ _ t) = (out, nodeSet)
    where 
    out = unlines [ "node[shape=oval];"
                            , string]

    (string, nodeSet) = runState (helper $ idToNode t) S.empty

    adj = (+ nodeOffset)
    idToNode = adj &&& accessAt wodd

    mkLabel lbl = "[label=\"" ++ lbl ++ "\"];"

    helper (thisId, Terminal) = return $
        -- switch to rectangle nodes for the leaf, before going back to ovals.
        unlines [ "node[shape=rectangle];"
                , show thisId
                , "[label=\" \"]"
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
                let idStr = show $ thisId 
                return $ unlines $ 
                   (idStr ++ mkLabel (show vid)) : 
                     concat [ [s, idStr ++ "->" ++ show (adj i) ++ mkLabel (show v) ] 
                                | (v, _, i) <- U.toList arcs | s <- strs]

woddCollectionToDot :: Show v => WoddCollection v -> String
woddCollectionToDot wc = wrapDigraph $ unlines $ 
    snd $ foldl go (0, []) (Foldable.toList wc) 
    where go (i, xs) w = let (out, set) = woddToDot' i w 
                             i' = S.findMax set
                         in (i'+ 1, out : xs) 

writeDot :: ToDot x => x -> IO FilePath
writeDot x = withSystemTempDirectory "wodd_dot" 
    $ \dir -> do -- (dotPath, hdot) <- openTempFile dir "wodd_dot.dot"
                 -- hClose hdot
                 let dotPath = ".wodd_dot"
                 writeFile ".wodd_dot" $ toDot x
                 -- contents <- readFile dotPath
                 --putStrLn contents
                 return dotPath

writeDotPdf :: FilePath -> IO ExitCode 
writeDotPdf path = rawSystem "dot" ["-Tpdf", path, "-o", ".wodd_pdf"]

displayDot :: ToDot x => x -> IO ExitCode
displayDot x = do dotPath <- writeDot x
                  dotStatus <- writeDotPdf dotPath
                  if dotStatus == ExitSuccess 
                     then rawSystem "open" [".wodd_pdf"]
                     else error "Data.WODD.Display.displayDot: ./dot return ExitFailure."
                  return dotStatus
