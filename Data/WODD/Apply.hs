
module Data.WODD.Apply where

import Prelude

import qualified Data.Vector.Unboxed as U
import Control.Monad
--import qualified Data.IntMap.Strict as IntMap
import Control.Monad.State.Strict()

import Data.WODD.Core
import Data.WODD.Types 
import qualified Data.WODD.Types as T (fromList)

import Data.Hashable

apply :: Eq v
        => (Arc -> Arc -> Index -> Arc)
        -> Order v
        -> WODD v 
        -> WODD v
        -> WODD v
apply op ord x y = make (handle (top x) (top y))
    where 

        --handle :: Index -> Index -> State (WODD v) Index
        handle ix iy = case (accessAt x ix, accessAt y iy) of 
                (Terminal, Terminal) -> register Terminal
                (Terminal, n@Branch{}) -> register n
                (n@Branch{}, Terminal) -> register n
                (Branch vx arcsx, Branch vy arcsy) 
                    -> case comp ord vx vy of 
                        LT -> do 
                            arcs' <- U.mapM (go ix) arcsy
                            register $ Branch vy arcs'
                        GT -> do 
                            arcs' <- U.mapM (go iy) arcsx
                            register $ Branch vx arcs'             
                        EQ -> do 
                            let v = if vx == vy then vx else error "Data.WODD.Apply.product/handle"
                            arcs' <- fmap U.fromList $ zipArcs (U.toList arcsx) (U.toList arcsy)
                            register $ Branch v arcs'  

        go iy (v, w, ix)   = do i' <- handle ix iy
                                return (v, w, i')

        combineArcs a1@(_, _, i1) a2@(_, _, i2) = do i <- handle i1 i2
                                                     return $ op a1 a2 i

        --zipArcs :: [Arc] -> [Arc] -> State (WODD v) [Arc]
        zipArcs [] zs = return zs
        zipArcs zs [] = return zs
        zipArcs (a1@(v1, w1, i1):xs1) zs2@(a2@(v2, w2, i2):xs2)
            | v1 < v2   = do arcs <- zipArcs xs1 zs2
                             return $ (v1, w1, i1): arcs 
            | v1 == v2  = do a <- combineArcs a1 a2
                             arcs <- zipArcs xs1 xs2
                             return $ a : arcs 
            | v1 > v2   = do arcs <- zipArcs xs1 xs2
                             return $ (v2, w2, i2) : arcs 

product :: Eq v => Order v -> WODD v -> WODD v -> WODD v
product = apply $ \(v1, w1, _) (v2, w2, _) i
                    -> (v1, w1 * w2, i) 

data L = L Int deriving (Eq)
instance Show L where
    show (L i) = 'x' : show i

instance Hashable L where
    hashWithSalt i (L l) = hashWithSalt i l

order = T.fromList $ fmap L [0..10]

test1 :: WODD L
test1 = make $ do 
    nodeX1 <- register $ Branch (L 1) $ U.fromList [(0, 0.4, termId), (1, 0.6, termId)]
    --nodeX2 <- register $ Branch (L 1) $ U.fromList [(0, 0.9, termId), (1, 0.1, termId)]
    node <- register $ Branch (L 2) $ U.fromList [(0, 0.3, nodeX1), (1, 0.3, termId)]
    return node





