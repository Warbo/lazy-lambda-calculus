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

data Term a = Const a
            | Var Nat
            | Lam (Term a)
            | Term a :@ Term a
              deriving (Show, Typeable, Data)

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

data Val a = V (Term a)
           | F (Val a -> Partial (Val a))

instance Show a => Show (Val a) where
  show (V t) = "(V " ++ show t ++ ")"
  show (F f) = "(F <function>)"

instance Eq a => Eq (Val a) where
  (V x) == (V y) = x == y
  (F x) == (F y) = error "Cannot compare function Vals"
  _     == _     = False

vc = V . Const
fapp (F f) x = f x
fapp  _    _ = error "Cannot apply V as a function"

type Env a = [Val a]

eval' :: Term a -> Env a -> Partial (Val a)
eval' (Const c) env = return (V (Const c))
eval' (Var n)   env = return $ case lookUp' env n of
                                 Just x  -> x
                                 Nothing -> error $ "Lookup " ++ show n
eval' (Lam f)   env = return (F (\a -> eval' f (a:env)))
eval' (l :@ r)  env = do F k <- eval' l env
                         a   <- eval' r env
                         Later (k a)

eval t | closed t  = eval' t []
eval t | otherwise = error "Can only evaluate closed Terms"

evalN n = force n . eval

closed' :: Nat -> Term a -> Bool
closed' n (Const _) = True
closed' n (Var   m) = m < n
closed' n (Lam   f) = closed' (S n) f
closed' n (l :@ r)  = closed' n l && closed' n r

closed :: Term a -> Bool
closed = closed' 0

-- Lambda calculus inside lambda calculus, using Morgensen-Scott encoding

-- Data constructors

-- Booleans

selfTrue  = Lam (Lam 1)
selfFalse = Lam (Lam 0)

-- Natural numbers
selfZ   = Lam (Lam 1)
selfS x = Lam (Lam (0 :@ x))

-- Partial monad
selfNow   x = Lam (Lam (1 :@ x))
selfLater x = Lam (Lam (0 :@ x))

-- Lists
selfNil       = Lam (Lam 1)
selfCons x xs = Lam (Lam (0 :@ x :@ xs))

-- Terms
selfVar n   = Lam (Lam (Lam (2 :@ n)))
selfApp x y = Lam (Lam (Lam (1 :@ x :@ y)))
selfLam x   = Lam (Lam (Lam (0 :@ (Lam x))))

-- Church encode a numeral
church :: Nat -> Term a
church x = let f 0 = 0
               f n = 1 :@ f (n - 1) in
           Lam (Lam (f x))

-- Morgensen-Scott encode a Term
mse :: Term a -> Term a
mse (Var x)  = selfVar (church x)
mse (x :@ y) = selfApp (mse x) (mse y)
mse (Lam x)  = selfLam (mse x)

{-
Lookup an element in a List. We can ignore empty lists:

selfLookup (selfCons h t)  selfZ    = h
selfLookup (selfCons h t) (selfS n) = selfLookup t n

We can abstract out the recursion using a Y combinator:

selfLookup = let f g (selfCons h t)  selfZ    = h
                 f g (selfCons h t) (selfS n) = g t n in
             zComb f

By definition 'selfCons h t A B = B h t', which eliminates the pattern-match:

selfLookup = let f g l  selfZ    = l id (\a b -> a)
                 f g l (selfS n) = l id (\a b -> g b n) in
             zComb f

By definition 'selfZ A B = A' and 'selfS n A B = B n', which eliminates the
remaining pattern-match:

selfLookup = let f g l m = m (l id (\a b -> a))
                             (\n -> l id (\a b -> g b n)) in
             zComb f

Now we can put everything inline:

selfLookup = zComb (\g l m -> m (l id (\a b -> a))
                                (\n -> l id (\a b -> g b n))

-}

-- Y combinator
yComb = Lam (Lam (1 :@ (0 :@ 0)) :@ Lam (1 :@ (0 :@ 0)))

-- Z combinator
zComb = Lam ((Lam (1 :@ (Lam (1 :@ 1 :@ 0)))) :@ (Lam (1 :@ (Lam (1 :@ 1 :@ 0)))))
