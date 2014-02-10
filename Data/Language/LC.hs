{-# Language FlexibleInstances          #-}
{-# Language MultiParamTypeClasses      #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language StandaloneDeriving         #-}
{-# Language DeriveDataTypeable         #-}
{-# Language DeriveFunctor              #-}
{-# Language DeriveFoldable             #-}
{-# Language DeriveTraversable          #-}

module Data.Language.LC where

import Control.Monad.Partial
import Data.Nat
import Data.Data
import Data.Typeable

-- Lambda calculus terms, with De Bruijn indices and opaque constants
data Term a = Const a
            | Var Nat
            | Lam (Term a)
            | Term a :@ Term a
              deriving (Show, Typeable, Data)

-- This lets us abbreviate "Var 0", "Var 1", etc. to "0" "1", etc.
instance Num (Term a) where
  (+)    = error "Cannot add Terms"
  (-)    = error "Cannot subtract Terms"
  (*)    = error "Cannot multiply Terms"
  abs    = error "Cannot abs a Term"
  negate = error "Cannot negate a Term"
  signum = error "Cannot signum a Term"
  fromInteger = Var . fromInteger

instance Eq a => Eq (Term a) where
  (Const x)  == (Const y)  = x == y
  (Var   x)  == (Var   y)  = x == y
  (Lam   x)  == (Lam   y)  = x == y
  (l1 :@ r1) == (l2 :@ r2) = l1 == l2 && r1 == r2

-- The results of evaluation: either a value or a closure
data Val a = C a
           | F (Partial (Val a) -> Partial (Val a))

instance Show a => Show (Val a) where
  show (C x) = "(C " ++ show x ++ ")"
  show (F f) = "(F <function>)"

instance Eq a => Eq (Val a) where
  (C x) == (C y) = x == y
  (F x) == (F y) = error "Cannot compare function Vals"
  _     == _     = False

instance Functor Val where
  fmap f (C x) = C (f x)
  fmap f (F g) = error "Cannot map embedded function"

valMap :: (a -> a) -> Val a -> Val a
valMap f (C x) = C (f x)
valMap f (F g) = F ((fmap (valMap f)) . g)

valLift :: (a -> a) -> Val a
valLift f = F (fmap (fmap f))

-- Apply a function Val to an argument
($$) (F f) x = f (Now (C x))
($$) (C f) x = error "Cannot apply C as a function"

-- Evaluation environment
type Env a = [Partial (Val a)]

-- Fully evaluates a Term, using Partial to handle nontermination
eval' :: Term a -> Env a -> Partial (Val a)
eval' (Const c) env = Now (C c)
eval' (Var   n) env = let Just x = lookUp env n in x
eval' (Lam   f) env = Now (F (\a -> eval' f (a:env)))
eval' (f :@  x) env = do F f' <- eval' f env
                         Later (f' (eval' x env))

-- Entry point for eval'
eval t | closed t  = eval' t []
eval t | otherwise = error "Can only evaluate closed Terms"

-- Abbreviation for performing N steps
evalN n = force n . eval

-- Predicate for capping the number of free variables in a Term
freeVars :: Nat -> Term a -> Bool
freeVars n (Const _) = True
freeVars n (Var   m) = m < n
freeVars n (Lam   f) = freeVars (S n) f
freeVars n (l :@ r)  = freeVars n l && freeVars n r

-- Predicate for ensuring a Term has no free variables
closed :: Term a -> Bool
closed = freeVars 0

-- Infinite loop
omega = Lam (0 :@ 0) :@ Lam (0 :@ 0)

-- Y combinator
yComb = Lam (Lam (1 :@ (0 :@ 0)) :@ Lam (1 :@ (0 :@ 0)))

-- Z combinator
zComb = Lam ((Lam (1 :@ (Lam (1 :@ 1 :@ 0)))) :@ (Lam (1 :@ (Lam (1 :@ 1 :@ 0)))))
