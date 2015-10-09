module Control.Monad.Partial where

import Data.Nat

-- Partial results are either a value (Now) or a partial result (Later).
data Partial a = Now a
               | Later (Partial a)
                 deriving (Show, Eq)

-- Delay mapping until we have a result (if ever).
instance Functor Partial where
  fmap f (Now   x) = Now   (f x)
  fmap f (Later x) = Later (fmap f x)

-- Compose functions once the first one halts (if ever)
instance Monad Partial where
  return = Now
  (Now   x) >>= f = f x
  (Later x) >>= f = Later (x >>= f)

-- See if we have a result yet
finished (Now _) = True
finished _       = False

-- Give a computation N steps to halt
force :: Nat -> Partial a -> Maybe a
force 0 _         = Nothing
force n (Now   x) = Just x
force n (Later x) = force (n-1) x

-- Conservative decision procedure
trueIn n x = fromMaybe False (force n x)

-- Lax decision procedure
notFalseIn n x = fromMaybe True (force n x)
