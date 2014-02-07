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
import Test.LazySmallCheck2012.Core hiding (Term)

selfTestMap :: Map String Test
selfTestMap = fromList [

                ("()",
                 Test $ \x -> let u = mse () :@ Const x in
                              closed (u :: Term Nat) ==>
                              equalIn 2 x u),

                ("true",
                 Test $ \n x y -> let t = mse True :@ Const x :@ y in
                                  closed (t :: Term Nat) ==>
                                  notDiffIn n x t),

                ("false",
                 Test $ \n x y -> let f = mse False :@ x :@ Const y in
                                  closed (f :: Term Nat) ==>
                                  notDiffIn n y f),

                ("z",
                 Test $ \n x y -> let z = mse Z :@ Const x :@ y in
                                  closed (z :: Term Nat) ==>
                                  notDiffIn n x z),
{-
                ("s",
                 Test $ \n m -> let s = mse (S m)
                                    wrap  Z    x = x :@ Const Z
                                    wrap (S n) x = wrap n x :@ Const (S n) in
                                notDiffIn n 0 (wrap m s)),
-}
                ("zFalse",
                 Test $ \x -> equalIn 5 x (zComb :@ mse False :@ Const (x :: Int)))
              ]

selfTest  = testRunner  selfTestMap
selfTests = testsRunner selfTestMap


foo n m = let s = mse (S m)
              {-wrap  Z    x = x :@ Const 0 :@ Const 1
              wrap (S n) x = wrap n x :@ Const (S n) :@ Const (S (S n)) in-}
              wrap x y = y :@ Lam 0 :@ Lam 0 :@ Const 4 :@ Const 6 in
          force n (eval (wrap m s))
