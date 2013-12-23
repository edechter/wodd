{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE CPP #-}

module Data.WODD.Display where

--- Imports -----------------------------------------
import Prelude
import System.IO.Temp 
import System.FilePath()
import System.IO
import System.Exit
import System.Cmd (rawSystem)
import Control.Arrow ( (&&&) )

import Data.IntMap.Strict()
import qualified Data.IntMap.Strict as IntMap

import qualified Data.Vector.Unboxed as U

import Data.Set()
import qualified Data.Set as S

import Control.Monad.State.Strict
   (evalState, gets, modify)

-----------------------------------------------------

import Data.WODD.Core
import Data.WODD.Types
import Data.WODD.Apply()

-- | toDot outputs a string in format suitable for input to the "dot" program
-- from the graphviz suite.
toDot :: (Show v) => WODD v -> String
toDot (WODD idmap _ _) | IntMap.null idmap = "digraph BDD {}" 
toDot wodd@(WODD _ _ t) =
    unlines [ "digraph BDD {"
            -- Start in oval mode
            , "node[shape=oval];"
            , evalState (helper $ idToNode t) S.empty
            , "}"
            ]
  where
    idToNode = id &&& accessAt wodd

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
                   (idStr ++ mkLabel (show vid)) : 
                     concat [ [s, idStr ++ "->" ++ show i ++ mkLabel (show v) ] 
                                | (v, _, i) <- U.toList arcs | s <- strs]


displayDot :: Show v => WODD v -> IO ExitCode
displayDot wodd = withSystemTempDirectory "wodd_dot" 
    $ \dir -> do (dotPath, hdot) <- openTempFile dir "wodd_dot.dot"
                 hClose hdot
                 writeFile dotPath $ toDot wodd
                 dotStatus <- rawSystem "dot" ["-Tpdf", dotPath, "-o", ".wodd_pdf"]
                 if dotStatus == ExitSuccess 
                    then rawSystem "open" [".wodd_pdf"]
                    else error "Data.WODD.Display.displayDot: ./dot return ExitFailure."
                 return dotStatus