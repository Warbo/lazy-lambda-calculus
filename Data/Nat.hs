{-# Language GeneralizedNewtypeDeriving #-}
{-# Language StandaloneDeriving         #-}
{-# Language DeriveDataTypeable         #-}
{-# Language DeriveFunctor              #-}
{-# Language DeriveFoldable             #-}
{-# Language DeriveTraversable          #-}

module Data.Nat where

import Data.Data
import Data.Typeable

-- Peano-style natural numbers
data Nat = Z
         | S Nat
           deriving (Eq, Typeable, Data)

instance Ord Nat where
  (<=)  Z     Z    = True
  (<=)  Z    (S _) = True
  (<=) (S x)  Z    = False
  (<=) (S x) (S y) = x <= y

natToInteger :: Nat -> Integer
natToInteger Z = 0
natToInteger x = let f n  Z    = n
                     f n (S x) = f (n+1) x in
                 f 0 x

natToInt :: Nat -> Int
natToInt = fromInteger . natToInteger

instance Show Nat where
  show = show . natToInteger

instance Num Nat where
  x + y = fromInteger (natToInteger x + natToInteger y)
  x * y = fromInteger (natToInteger x * natToInteger y)

  (-) x      Z    = x
  (-) (S x) (S y) = x - y
  (-) Z      _    = error "Subtraction was truncated"

  negate = error "Cannot negate a Nat"

  abs = id

  signum  Z    = Z
  signum (S _) = S Z

  fromInteger = let f 0 = Z
                    f n = S (f (n - 1)) in
                f . abs

instance Real Nat where
  toRational = toRational . toInteger

instance Integral Nat where
 quotRem x y = let x' = toInteger x'
                   y' = toInteger y'
                   (q, r) = quotRem x' y' in
               (fromInteger q, fromInteger r)

lookUp' :: [a] -> Nat -> Maybe a
lookUp' []      _    = Nothing
lookUp' (x:xs)  Z    = Just x
lookUp' (x:xs) (S n) = lookUp' xs n

lookUp :: Show a => [a] -> Nat -> a
lookUp l n = case lookUp' l n of
                  Just x  -> x
                  Nothing -> error ("lookUp " ++ show l ++ " " ++ show n)
