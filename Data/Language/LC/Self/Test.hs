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
import Test.LazySmallCheck2012.Core hiding (Term, C)

encodeDecode :: Encodable a => a -> Partial (Val a)
encodeDecode = decode . mse

encodeDecodeIn n x = (force n (encodeDecode x)) == Just (C x)

selfTestMap :: Map String Test
selfTestMap = fromList [

                ("()",
                 Test $ encodeDecodeIn 2 ()),

                ("Bool",
                 Test $ \b -> encodeDecodeIn 3 (b :: Bool)),

                ("Nat",
                 Test $ \n -> encodeDecodeIn (3+n) (n :: Nat))


              ]

decodeNat n x = closed x ==> evalN n (mse (S Z) :@ Const Z :@ x) /= Just (C (S Z))



selfTest  = testRunner  selfTestMap
selfTests = testsRunner selfTestMap


foo n m = let s = mse (S m)
              {-wrap  Z    x = x :@ Const 0 :@ Const 1
              wrap (S n) x = wrap n x :@ Const (S n) :@ Const (S (S n)) in-}
              wrap x y = y :@ Lam 0 :@ Lam 0 :@ Const 4 :@ Const 6 in
          force n (eval (wrap m s))
