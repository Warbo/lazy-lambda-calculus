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

module Data.Language.LC.Self.Test where

import Control.Monad.Partial
import Data.Nat
import Data.Data
import Data.Language.LC
import Data.Language.LC.Test
import Data.Language.LC.Self
import Data.Map
import Data.Maybe
import Data.Typeable
import Data.Test
import Test.LazySmallCheck2012 hiding (Nat, Term, Const)
import Test.LazySmallCheck2012.Core hiding (Term, C, mkTest)

import Debug.Trace

data Wrap a = W a deriving (Eq, Show, Data, Typeable)
instance Serial a => Serial (Wrap a) where
  series = cons1 W

instance (Encodable a, Show a) => Encodable (Wrap a) where
  --mse (W x) = Lam (0 :@ mse x)
  --unmse x = eval' (0 :@ 1) [Now x,
  --                          Now (F (fmap (valMap W) . (>>= unmse)))]

foo :: (Val a -> Partial (Val a)) -> Val (Wrap a) -> Partial (Val (Wrap a))
foo m f = eval' (0 :@ 1) [Now f, Now (F (>>= bar m))]

bar :: (Val a -> Partial (Val a))

encodeDecode :: Encodable a => a -> Partial (Val a)
encodeDecode = decode . mse

encodeDecodeIn n x = (force n (encodeDecode x)) == Just (C x)

mkTest :: (Serial a, Eq a, Encodable a) => (a -> Nat) -> Test
mkTest f = Test $ \x -> encodeDecodeIn (f x) x

selfTestMap :: Map String Test
selfTestMap = fromList [

                -- Finite enumerations
                ("()",   mkTest (const 2 :: ()   -> Nat)),

                ("Bool", mkTest (const 3 :: Bool -> Nat)),

                -- Unbounded enumerations
                ("Nat",  mkTest (3+)),

                -- Finite wrappers
                ("Wrap ()", mkTest (const 3 :: Wrap () -> Nat))

                --("Partial ()", mkTest (((2+) . laterCount) :: Partial () -> Nat))

              ]

--decodeNat n x = closed x ==> evalN n (mse (S Z) :@ Const Z :@ x) /= Just (C (S Z))



selfTest  = testRunner  selfTestMap
selfTests = testsRunner selfTestMap
