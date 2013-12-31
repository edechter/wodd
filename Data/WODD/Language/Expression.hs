{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ParallelListComp #-}

module Data.WODD.Language.Expression where

import Prelude hiding (elem, foldl)
import Data.WODD.Types 
import Data.WODD.Core
import qualified Data.WODD.Core as W (empty)
import Data.WODD.Display
import Data.Ratio
import Data.List (findIndex) 
import Data.Foldable (foldl)

import Data.Sequence ((<|), (|>),  (><))
import qualified Data.Sequence as Seq

import qualified Data.List as List 
import Data.Maybe
import Data.Monoid
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Arrow (second)
import Control.Monad.State.Strict
     (State(), runState, evalState)


infixl 7 |-> 
infixr 0 <== 

-- A Finite type is a mapping from [0..n] to a set of objects.
-- Ideally, we would like the number of objects to be a parameter of the type. 

class (Eq e, Show e) => Finite e where 
    -- | Type class for Finite data types. Minimal definition is `elem`
    elem      :: [e]


numElem :: (Integral a) => [e] -> a
numElem es = fromIntegral $ length es

fromIndex :: Integral a => a -> [e] -> e
fromIndex a es | a >= numElem es = error "Expression2.fromIndex: requested index larger than numElem."
               | otherwise    = es !! (fromIntegral a)

toIndex   :: (Show e, Eq e, Integral a) => e -> [e] -> a
toIndex e es = case findIndex (==e) es of 
                Just i -> fromIntegral i 
                Nothing  -> error $ "Expression2.toIndex: cannot find element " ++ show e ++ "."
    

instance Finite Bool where 
    elem = [True, False]

data V a where
    V :: Finite e => String -> V e

instance Show (V a) where
  show (V s) = s

data UV where 
    UV :: V a -> UV  

instance Show UV where 
    show (UV v) = show v

instance Eq UV where 
    UV (V s) == UV (V s') = s == s'

data RV a where 
     RV :: [(e, Double)] -> RV e

instance Finite e => Show (RV e) where 
    show (RV xs) = show xs

instance Monoid (RV a) where 
    (RV xs) `mappend` (RV ys) = RV $ xs ++ ys
    mempty = RV []

a |-> b = [(a, b)]

data Expr a where 
  Ref    :: Finite e => V e -> Expr e
  Case   :: Finite e => V e -> [(e, Expr b)] -> Expr b
  Choice :: Finite e => RV e -> Expr e
  And    :: Expr Bool -> Expr Bool -> Expr Bool
  Or    ::  Expr Bool -> Expr Bool -> Expr Bool

instance Show a => Show (Expr a) where 
    show (Ref v) = show v
    show (Case v xs) = "case " ++ show v ++ " " ++ show xs
    show (Choice xs) = "choice " ++ show xs 
    show (And a b)   = show a ++ "&&" ++ show b
    show (Or a b)    = show a ++ "||" ++ show b

data Statement where 
    Assign :: Finite e => V e -> Expr e -> Statement
    Sequence :: Statement -> Statement -> Statement
    Null     :: Statement

instance Show Statement where
  show (Assign v e) = show v ++ " := " ++ show e
  show (Sequence s1 s2) = show s1 ++ "\n" ++ show s2
  show (Null) = []

data Stmt a = Stmt ((Int, Statement) -> (a, (Int, Statement)))

instance Monad Stmt where
  return a = Stmt $ \ s -> (a, s)
  Stmt f >>= st = Stmt $ \ (i, s) 
                    -> let (a, (i', s')) = f (i, s) --  first pass through f
                           Stmt g = st a -- produce the next stmt
                       in g (i', s')

get :: Stmt (Int, Statement)
get = Stmt $ \ a -> (a, a)

put :: (Int, Statement) -> Stmt ()
put s = Stmt $ \ _ -> ((), s)


statement :: Statement -> Stmt ()
statement a = Stmt $ \ (i, statement) -> ((), (i, Sequence statement a))

runStmt  :: Int -> Stmt a -> (a, (Int, Statement))
runStmt i (Stmt f) = f (i, Null)

evalStmt :: Int -> Stmt () -> (Int, Statement)
evalStmt i (Stmt f) = snd $ f (i, Null)

fromStmt :: Stmt a -> Statement 
fromStmt (Stmt f) = snd . snd $ f (0, Null)

--class Assign a where 
--  (<==) :: V a -> Expr a -> Stmt ()

--instance Finite e => Assign e where 
a <== b = statement $ Assign a b

choice :: Finite e => [(e, Double)] -> Expr e
choice = Choice . RV 

bern :: Double -> Expr Bool
bern p = choice [(True, p), (False, 1 - p)]

uniform :: Finite e => [e] -> RV e
uniform es = RV $ [(x, 1.0 / n) | x <- es ]
             where n = fromIntegral $ length es

if_ :: V Bool -> Expr b -> Expr b -> Expr b
if_ v ifTrue ifFalse = Case v [(True, ifTrue), (False, ifFalse)]

(||.) = Or

switch   :: Finite e => V e -> [(e, Expr b)] -> Expr b
switch = Case

wp1 :: Finite e => e -> Expr e
wp1 e = choice [(e, 1)]

ref :: Finite e => V e -> Expr e
ref v = switch v [(e, wp1 e) | e <- elem]

prog = do let x = V "x" :: V Bool 
              y = V "y" :: V Bool
              z = V "z" :: V Bool
          y <== bern 0.3
          x <== if_ y (bern 0.4) (bern 0.9)
          z <== (Ref x ||. Ref y)

data Component = One | Two | Three deriving (Show, Eq)
instance Finite Component  where 
    elem = [One, Two, Three]

data Color     = Red | Green deriving (Show, Eq)
instance Finite Color  where
    elem = [Red, Green]

instance Finite Int where 
    elem = [0..]

mixture = do let z = V "z" :: V Component
                 x = V "x" :: V Color
                 y = V "y" :: V Color
                 --w = V "w" :: V (H Int)
                 bernColor p = choice [(Red, p), (Green, 1 - p)]
             z <== (Choice (uniform elem) :: Expr Component)
             --w <== (Choice (uniform elem) :: Expr (H Int))
             y <== switch z $    One   |-> bernColor 0.2
                              <> Two   |-> bernColor 0.4
                              <> Three |-> bernColor 0.6

mkX i = V ("x" ++ show i) :: V Bool
y = V "y" :: V Bool
xGiveny = switch y $ True  |-> bern 0.5 
                <>  False |-> bern 0.8

iid = do y <== bern 0.2
         sequence $ 
            do x <- [mkX i | i <- [0..10]] 
               return $ do x <== xGiveny 
         return ()

boolVar s = V s :: V Bool
aOrb = let z = boolVar "z"
           a = boolVar "a"
           b = boolVar "b"
           c = boolVar "c"
       in do z <== bern 0.2 
             a <== bern 0.3 
             b <== bern 0.6
             c <== if_ z 
                      (ref a)
                      (ref b) 

uniformI low hi = RV $ [(i, 1.0/n) | i <- [low, hi]]
    where n = fromIntegral $ hi - low

ors = let z = V "z" :: V Int
          xs = [mkX i | i <- [0..10]]
          c  = boolVar "c"
      in do z <== Choice $ uniformI 0 10
            sequence $ [x <== bern 0.2 | x <- xs]
            c <== switch z $ [(i, ref x ) | x <- xs | i <- [0..10]]


type UVSeq = Seq.Seq UV

addVar :: UVSeq -> UV -> UVSeq 
addVar vs v = case Seq.elemIndexL v vs of 
                Nothing -> vs |> v
                Just _  -> vs   

varToInt :: UVSeq -> UV -> Int
varToInt vs v = case Seq.elemIndexL v vs of 
                    Nothing -> error $ "Data.WODD.Language.Expression2.varToInt: " ++ 
                                    "cannot find variable " ++ show v ++ " in var list " ++
                                    show vs
                    Just i -> i + 1
                    

exprVars :: Expr a -> UVSeq
exprVars expr = go Seq.empty expr 
    where go :: UVSeq -> Expr a -> UVSeq
          go vs (Ref v) = addVar vs (UV v)
          go vs (Case v xs) = let vs' = addVar vs (UV v)
                              in foldl go vs' (map snd xs)
          go vs (Choice (RV xs)) = vs


statementVars :: Statement -> UVSeq
statementVars (Assign v e) = addVar (exprVars e) (UV v)
statementVars (Sequence s1 s2) = foldl addVar (statementVars s1) (statementVars s2) 
statementVars Null = Seq.empty

stmtVars_ :: Stmt UVSeq
stmtVars_ = do (i, s) <- get 
               return $ statementVars s

stmtVars :: Stmt a -> UVSeq 
stmtVars stmt = fst $ runStmt 0 $ stmt >> stmtVars_


exprToWODD :: UVSeq -> UV -> Expr a -> State (WODD Int) Index
exprToWODD vs v (Choice (RV xs)) 
    = register . Branch vInt . arcsFromList 
        $ [arcToLeaf (toIndex l elem) d  | (l, d) <- xs]
    where vInt = varToInt vs v
exprToWODD vs v (Case w xs)
    = do ys <- mapM (exprToWODD vs v . snd) xs
         let ls = map fst xs
         register . Branch wInt . arcsFromList  
            $ [arcToNode (toIndex l elem) 1 n | (l, n) <- zip ls ys]
    where wInt = varToInt vs (UV w)

statementToWODD :: UVSeq -> Statement -> WoddCollection Int
statementToWODD vs (Assign v e) = let wodd = make $ exprToWODD vs (UV v) e  
                                  in Seq.singleton wodd
statementToWODD vs (Sequence s1 s2) 
    = let wc  =  statementToWODD vs s1 
          wc' =  statementToWODD vs s2 
      in wc >< wc'
statementToWODD vs Null = Seq.empty

main = let w = V "w" :: V Component
           z = V "z" :: V Component
           t = Choice (uniform elem) :: Expr Component
           y = switch z $    One   |-> bern 0.2
                          <> Two   |-> bern 0.4
                          <> Three |-> bern 0.6
           y1 = ref z
           vs = (UV w) <| exprVars y1
           assignment = Assign w (ref z)
           wodd = statementToWODD (statementVars assignment) assignment
       in 
        do displayDot wodd







