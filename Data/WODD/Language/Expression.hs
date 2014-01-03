{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ParallelListComp #-}

module Data.WODD.Language.Expression where

import Prelude hiding (elem, foldl, or)
import Data.WODD.Types
import Data.WODD.Core
import qualified Data.WODD.Core as W (empty)
import Data.WODD.Display
import Data.Ratio
import Data.List (findIndex, elemIndex)
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


-- infix operators ------
infixl 7 |->, |->>
infixr 0 <== , <=|, <.>
-------------------------

-- | Typed variables.
-- | A variable is identified by a string.
data V a where
    V :: Finite e => String -> V e

instance Show (V a) where
  show (V s) = s

-- | Untyped wrapper for variables.
data UV where
    UV :: V a -> UV

instance Show UV where
    show (UV v) = show v

instance Eq UV where
    UV (V s) == UV (V s') = s == s'


-- | Discrete random variable.

data RV a where
     RV :: [(e, Double)] -> RV e

instance Finite e => Show (RV e) where
    show (RV xs) = show xs

instance Monoid (RV a) where
    (RV xs) `mappend` (RV ys) = RV $ xs ++ ys
    mempty = RV []

(|->>) :: Finite e => e -> Double -> RV e
e |->> d = RV [(e, d)]

-- | A data type for expressions.
data Expr a where
  Case   :: Finite e => V e -> [(e, Expr b)] -> Expr b
  Choice :: Finite e => RV e -> Expr e

instance Show a => Show (Expr a) where
    show (Case v xs) = "case " ++ show v ++ " " ++ show xs
    show (Choice xs) = "choice " ++ show xs

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
toIndex e es = case elemIndex e es of
                Just i -> fromIntegral i
                Nothing  -> error $ "Expression2.toIndex: cannot find element " ++ show e ++ "."


instance Finite Bool where
    elem = [True, False]

instance Finite Int where
    elem = [0..]

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
put s = Stmt $ const ((), s)

freshVar :: Finite e => Stmt (V e)
-- | Return a fresh variable "x_<i>" and increment variable counter.
freshVar = do (i, s) <- get
              put $ (i+1, s)
              return $ V $ "x_" ++ show i

statement :: Statement -> Stmt ()
statement a = Stmt $ \ (i, statement) -> ((), (i, Sequence statement a))

runStmt  :: Int -> Stmt a -> (a, (Int, Statement))
runStmt i (Stmt f) = f (i, Null)

evalStmt :: Int -> Stmt () -> (Int, Statement)
evalStmt i (Stmt f) = snd $ f (i, Null)

fromStmt :: Stmt a -> Statement
fromStmt (Stmt f) = snd . snd $ f (0, Null)


---------------------------------------------------------
-- Language combinators and methods ---------------------
---------------------------------------------------------

(<==) :: Finite e => V e -> Expr e -> Stmt ()
a <== b = statement $ Assign a b

(<=|) :: Finite e 
      => V e            -- ^ variable, x
      -> Stmt (Expr e)  -- ^ a program generating computation s that returns an expression, e. 
      -> Stmt ()        -- ^ a program generating computation that program that performs s and adds an assignment that
                        --   binds e to variable x. 
-- | An assignment operator. 
a <=| b = do e <- b
             a <== e

(|->) :: Finite e => e -> Stmt (Expr a) -> [(e, Stmt (Expr a))]
a |-> st = [(a, st)]

(<.>) :: [(e, Stmt (Expr a))] -> [(e, Stmt (Expr a))] -> [(e, Stmt (Expr a))]
(<.>) = (++)

choice :: Finite e => [(e, Double)] -> Stmt (Expr e)
choice xs = return $ Choice $ RV xs

switch  :: Finite e => V e -> [(e, Stmt (Expr b))] -> Stmt (Expr b)
switch v xs = do es <- sequence ms
                 return $ Case v (zip ws es)
              where (ws, ms) = unzip xs

bern :: Double -> Stmt (Expr Bool)
bern p = choice [(True, p), (False, 1 - p)]

uniform :: Finite e => [e] -> Stmt (Expr e)
uniform es = return . Choice . RV $ [(x, 1.0 / n) | x <- es ]
             where n = fromIntegral $ length es

if_ :: V Bool -> Stmt (Expr b) -> Stmt (Expr b) -> Stmt (Expr b)
if_ v ifTrue ifFalse = do t <- ifTrue
                          f <- ifFalse
                          return $ Case v [(True, t), (False, f)]

boolBinOp :: (Bool -> Bool -> Bool) -> V Bool -> V Bool -> Stmt (Expr Bool)
boolBinOp f v1 v2
  = switch v1 $ True  |-> (switch v2 $ True  |-> wp1 (f True True)
                                   <>  False |-> wp1 (f True False))
            <>  False |-> (switch v2 $ True  |-> wp1 (f False True)
                                   <>  False |-> wp1 (f False False))

ifThenElse pred_st true_st false_st =
 do x <- freshVar
    x <=| pred_st
    if_ x true_st false_st

wp1 :: Finite e => e -> Stmt (Expr e)
wp1 e = choice [(e, 1)]

eq :: Finite e => V e -> Stmt (Expr e)
eq v = switch v [(e, wp1 e) | e <- elem]

(.~) :: Finite e => V e -> V e -> Stmt ()
v .~ w = v <=| switch w [(e, wp1 e) | e <- elem]

---------------------------------------------------------
---------------------------------------------------------

st1 ||. st2 = do e1 <- st1
                 e2 <- st2
                 x1 <- freshVar
                 x2 <- freshVar
                 x1 <== e1
                 x2 <== e2
                 boolBinOp (||) x1 x2

st1 &&. st2 = do e1 <- st1
                 e2 <- st2
                 x1 <- freshVar
                 x2 <- freshVar
                 x1 <== e1
                 x2 <== e2
                 boolBinOp (&&) x1 x2




prog = do let x = V "x" :: V Bool
              y = V "y" :: V Bool
              z = V "z" :: V Bool
          y <=| bern 0.3
          x <=| if_ y (bern 0.4) (bern 0.9)

data Component = One | Two | Three deriving (Show, Eq)
instance Finite Component  where
    elem = [One, Two, Three]

data Color     = Red | Green deriving (Show, Eq)
instance Finite Color  where
    elem = [Red, Green]


mixture = do let z = V "z" :: V Component
                 x = V "x" :: V Color
                 y = V "y" :: V Color
                 --w = V "w" :: V (H Int)
                 bernColor p = choice [(Red, p), (Green, 1 - p)]
             z <=| (uniform elem :: Stmt (Expr Component))
             y <=| switch z $     One   |-> bernColor 0.2
                              <.> Two   |-> bernColor 0.4
                              <.> Three |-> bernColor 0.6

mkX i = V ("x" ++ show i) :: V Bool
y = V "y" :: V Bool

xGiveny = switch y $ True  |-> bern 0.5
                  <>  False |-> bern 0.8

iid = do y <=| bern 0.2
         sequence $
            do x <- [mkX i | i <- [0..10]]
               return $ do x <=| xGiveny
         return ()

boolVar s = V s :: V Bool
aOrb = let z = boolVar "z"
           a = boolVar "a"
           b = boolVar "b"
           c = boolVar "c"
       in do z <=| bern 0.2
             a <=| bern 0.3
             b <=| bern 0.6
             c <=| if_ z
                      (eq a)
                      (eq b)

uniformI low hi = return $ Choice $ RV $ [(i, 1.0/n) | i <- [low, hi]]
    where n = fromIntegral $ hi - low

ors = let z = V "z" :: V Int
          xs = [mkX i | i <- [0..10]]
          c  = boolVar "c"
      in do z <=| uniformI 0 10
            sequence $ [x <=| bern 0.2 | x <- xs]
            c <=| switch z $ [(i, eq x ) | x <- xs | i <- [0..10]]

or' = let y = boolVar "y"
          z = boolVar "z"
          w = boolVar "w"
      in do y <=| bern 0.3
            z <=| bern 0.6
            w <=| eq y ||. eq z

andProg = let y = boolVar "y"
              z = boolVar "z"
              w = boolVar "w"
          in do y <=| bern 0.3
                z <=| bern 0.6
                w <=| ifThenElse (eq y &&. eq z) (bern 0.9) (wp1 True)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- Converting programs to WODDs ---------------------------------------------
------------------------------------------------------------------------------

type UVSeq = Seq.Seq UV

addVar :: UVSeq -> UV -> UVSeq
-- | Add a variable to the right end of a variable sequence. 
-- | Do nothing if variable is already in the sequence. 
addVar vs v = case Seq.elemIndexL v vs of
                Nothing -> vs |> v
                Just _  -> vs

varToInt :: UVSeq -> UV -> Int
-- | Map a variable to to an integer according to its location in the variable
-- sequence. This should be O(log n) with the Data.Seq data structure.
varToInt vs v = case Seq.elemIndexL v vs of
                    Nothing -> error $ "Data.WODD.Language.Expression2.varToInt: " ++
                                    "cannot find variable " ++ show v ++ " in var list " ++
                                    show vs
                    Just i -> i + 1


exprVars :: Expr a -> UVSeq
-- | Extract a variable sequence from an expression. Variables in order of
-- appearance in the expression
exprVars expr = go Seq.empty expr
    where go :: UVSeq -> Expr a -> UVSeq
          --go vs (Ref v) = addVar vs (UV v)
          go vs (Case v xs) = let vs' = addVar vs (UV v)
                              in foldl go vs' (map snd xs)
          go vs (Choice (RV xs)) = vs


statementVars :: Statement -> UVSeq
-- | Extract a variable sequence from a statement. 
statementVars (Assign v e) = addVar (exprVars e) (UV v)
statementVars (Sequence s1 s2) = foldl addVar (statementVars s1) (statementVars s2)
statementVars Null = Seq.empty

stmtVars_ :: Stmt UVSeq
stmtVars_ = do (i, s) <- get
               return $ statementVars s

stmtVars :: Stmt a -> UVSeq
-- | Extract a variable sequence from a program. 
stmtVars stmt = fst $ runStmt 0 $ stmt >> stmtVars_


exprToWODD :: UVSeq                    -- ^ A variable sequence, s. Must contain x. 
            -> UV                      -- ^ x, a variable.
            -> Expr a                  -- ^ an expression, e, describing the distribution of x.
            -> State (WODD Int) Index 
-- | Return a WODD ordered by s describing x distributed according to e. 
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

statementToWODD :: UVSeq                -- ^ A variable sequence, s. Must contain all variables occuring in p.  
                -> Statement            -- ^ p, a program. 
                -> WoddCollection Int   -- ^ A collection of WODDs, which, taken together, describes the distribution 
                                        --   of all variables occuring in s. 
statementToWODD vs (Assign v e) = let wodd = make $ exprToWODD vs (UV v) e
                                  in Seq.singleton wodd
statementToWODD vs (Sequence s1 s2)
    = let wc  =  statementToWODD vs s1
          wc' =  statementToWODD vs s2
      in wc >< wc'
statementToWODD vs Null = Seq.empty


main = let st = fromStmt andProg
           wc = statementToWODD (statementVars st) st
       in displayDot wc







