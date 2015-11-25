{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleContexts #-}
{-# Language ExistentialQuantification #-}
{-# Language DeriveDataTypeable #-}

module Control.Monad.Partial where

import Data.Data
import Data.Maybe
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

-- Delay applications until we have a function (if ever)
instance Applicative Partial where
  pure = Now
  (Now   f) <*> x =        f <$> x
  (Later f) <*> x = Later (f <*> x)

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
trueIn n x = fromMaybe False (force n x)

-- Lax decision procedure
notFalseIn n x = fromMaybe True (force n x)

-- We can use infinite proofs to get around type constraints
undefined' :: Partial a
undefined' = Later undefined'

cast' :: (Typeable a, Typeable b) => a -> Partial b
cast' x = fromMaybe undefined' (cast x)
