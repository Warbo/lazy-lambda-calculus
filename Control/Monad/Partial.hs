{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleContexts #-}
{-# Language ExistentialQuantification #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}

module Control.Monad.Partial where

import Data.Data
import Data.Nat
import Data.Typeable
import Test.LazySmallCheck2012 hiding (Nat, Term, Const)
import Test.LazySmallCheck2012.Core hiding (Term, C)

-- Partial results are either a value (Now) or a partial result (Later).
data Partial a = Now a
               | Later (Partial a)
                 deriving (Show, Eq, Typeable, Data)

-- Delay mapping until we have a result (if ever).
instance Functor Partial where
  fmap f (Now   x) = Now   (f x)
  fmap f (Later x) = Later (fmap f x)

-- Compose functions once the first one halts (if ever)
instance Monad Partial where
  return = Now
  (Now   x) >>= f = f x
  (Later x) >>= f = Later (x >>= f)

instance Serial a => Serial (Partial a) where
  series = cons1 Now \/ cons1 Later

-- See if we have a result yet
finished (Now _) = True
finished _       = False

laterCount (Now   _) = 0
laterCount (Later x) = S (laterCount x)

-- Give a computation N steps to halt
force :: Nat -> Partial a -> Maybe a
force 0 _         = Nothing
force n (Now   x) = Just x
force n (Later x) = force (n-1) x

-- Conservative decision procedure
trueIn n x = case force n x of
               Just b  -> b
               Nothing -> False

-- Lax decision procedure
notFalseIn n x = case force n x of
                   Just b  -> b
                   Nothing -> True
