{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Data.WODD.Language.Expression2 where

import Prelude
import Data.WODD.Types (WODD)
import Data.Ratio
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Applicative ((<$>), (<*>), (<*), (*>))

infixl 7 |-> 
infixr 0 <== 

class (Show t, Show e) => Finite t e | t -> e where 
    enum :: [e]

instance Finite Bool Bool where 
    enum = [True, False] 


data V a where
    V :: Finite t e => String -> V t

instance Show (V a) where
  show (V s) = s

data RV a where 
     RV :: [(e, Double)] -> RV t

instance (Finite t e, Show t, Show e) => Show (RV t e) where 
    show (RV xs) = show xs

instance Monoid (RV a) where 
    (RV xs) `mappend` (RV ys) = RV $ xs ++ ys
    mempty = RV []

a |-> b = [(a, b)]

data Expr a where 
  Ref    :: Finite t e => V t -> Expr t
  Case   :: Finite t e => V t -> [(e, Expr b)] -> Expr b
  Choice :: Finite t e => RV t -> Expr a
  And    :: Expr Bool -> Expr Bool -> Expr Bool
  Or    ::  Expr Bool -> Expr Bool -> Expr Bool

instance Show a => Show (Expr a) where 
    show (Ref v) = show v
    show (Case v xs) = "case " ++ show v ++ " " ++ show xs
    show (Choice xs) = "choice " ++ show xs 
    show (And a b)   = show a ++ "&&" ++ show b
    show (Or a b)    = show a ++ "||" ++ show b

data Statement where 
    Assign :: Finite t e => V t -> Expr t -> Statement
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

evalStmt :: Int -> Stmt () -> (Int, Statement)
evalStmt i (Stmt f) = snd $ f (i, Null)

class Assign a where 
  (<==) :: V a -> Expr a -> Stmt ()

instance Finite t e => Assign t where 
  a <== b = statement $ Assign a b

choice :: Finite t e => [(e, Double)] -> Expr a
choice = Choice . RV 

bern :: Double -> Expr Bool
bern p = choice [(True, p), (False, 1 - p)]

uniform :: Finite t e => RV t
uniform = RV $ [(x, 1.0 ) | x <- enum ]
          --n :: Double
          --n = 4.0


if_ :: V Bool -> Expr b -> Expr b -> Expr b
if_ v ifTrue ifFalse = Case v [(True, ifTrue), (False, ifFalse)]

(||.) = Or

switch = Case


prog = do let x = V "x" :: V Bool 
              y = V "y" :: V Bool
              z = V "z" :: V Bool
          y <== bern 0.3
          x <== if_ y (bern 0.4) (bern 0.9)
          z <== (Ref x ||. Ref y)

data Component = One | Two | Three deriving (Show, Eq)
instance Finite Component Component where 
    enum = [One, Two, Three]


data Color     = Red | Green deriving (Show, Eq)
instance Finite Color Color where
    enum = [Red, Green]

data H = H deriving Show
instance Finite H Int where 
    enum = [0..100]



mixture = do let z = V "z" :: V Component
                 x = V "x" :: V Color
                 y = V "y" :: V Color
                 bernColor p = choice [(Red, p), (Green, 1 - p)]
             z <== (Choice uniform :: Expr Component)
             y <== switch z $    One   |-> bernColor 0.2
                              <> Two   |-> bernColor 0.4
                              <> Three |-> bernColor 0.6











