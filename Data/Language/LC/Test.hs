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

module Data.Language.LC.Test where

import Control.Monad.Partial
import Data.Nat
import Data.Data
import Data.Language.LC
import Data.Map
import Data.Maybe
import Data.Typeable
import Test.LazySmallCheck2012 hiding (Nat, Term, Const)
import Test.LazySmallCheck2012.Core hiding (Term)

instance Serial Nat where
  series = cons0 Z \/
           cons1 S

-- Randomly generate terms
instance Serial a => Serial (Term a) where
  series = cons1 Var \/
           cons1 Lam \/
           cons2 (:@)

data Test = forall a. (Typeable a,
                       Data a,
                       Testable a) => Test a

runDepthCheck n (Test t) = depthCheck n t

fappC x = (`fapp` vc x)

tests :: Map String Test
tests = let i = Lam 0 :: Term Nat in
        fromList [
         ("evalCoterminates",
         Test $ \x n -> let x' = evalN (n `div` 2) (x :: Term ()) in
                        closed x ==> isJust x' || isNothing x'),

         ("noVarLeaks",
          Test $ \x n m -> closed (x :: Term ()) ==>
                           notFalseIn n (fmap (V (Var m) /=) (eval x))),

         ("omegaDiverges",
          Test $ \n -> evalN n (omega :: Term ()) == Nothing),

         ("evalFapp",
          Test $ \n m -> trueIn (n+1) (fmap (== vc m) (eval i >>= (`fapp` vc m)))),

         ("evalSteps",
          Test $ \n m -> trueIn (n+2) (fmap (== vc m) (eval (i :@ i) >>= (`fapp` vc m)))),

         ("idNormal",
          Test $ \n m -> trueIn (n+2) (fmap (== vc m) (eval (i :@ Const m)))),

         ("evalZFalse",
          Test $ \n m -> trueIn (n+5) (fmap (== vc (m :: Nat)) (eval (zComb :@ selfFalse :@ Const m)))),


         ("id",
          Test $ \n x -> trueIn (n+2) (fmap (== vc x) (eval (i :@ Const x)))),

         ("true",
          Test $ \n x y -> let t = selfTrue :@ Const x :@ y in
                           closed (t :: Term ()) ==>
                           notFalseIn n (fmap (== vc x) (eval t))),

         ("false",
          Test $ \n x y -> let f = selfFalse :@ x :@ Const y in
                           closed (f :: Term ()) ==>
                           notFalseIn n (fmap (== vc y) (eval f)))
    ]

runTests n = let next []                  = return ()
                 next ((name, Test t):xs) = do print name
                                               depthCheck n t
                                               next xs in
             next . toList $ tests

runTest n t = case Data.Map.lookup t tests of
                Nothing       -> error ("Could not find test " ++ t)
                Just (Test x) -> depthCheck n x

omega = Lam (0 :@ 0) :@ Lam (0 :@ 0)

{-
plus  Z    y = y
plus (S x) y = S (plus x y)

 Z    a b = a
(S x) a b = b (x a b)

plus f  Z    y = Z y _
plus f (S x) y = (S x) _ (\x -> f x y)
plus f x y = x y (\n -> f n y)
\f (\x (\y ((x y) (\n ((f n) y)))))
\  (\  (\  ((1 0) (\  ((3 0) 1)))))

plus = zComb :@ Lam (Lam (Lam (1 :@ 0 :@ (Lam (3 :@ 0 :@ 1)))))
testPlus x y = let sum = plus :@ church x :@ church y
                   result = force (2 * (x + y + 10)) (eval sum) in
               result == Just (church (x + y))
-}
