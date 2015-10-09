{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleContexts #-}
{-# Language ExistentialQuantification #-}

module Data.Language.LC.Test where

import Control.Monad.Partial
import Data.Nat
import Data.Data
import Data.Language.LC
import Data.Map
import Data.Maybe
import Data.Test
import Data.Typeable
import Test.LazySmallCheck2012 hiding (Nat, Term, Const)
import Test.LazySmallCheck2012.Core hiding (Term, C)

-- Randomly generate terms
instance Serial a => Serial (Term a) where
  series = cons1 Var \/
           cons1 Lam \/
           cons2 (:@)

equalIn   n x y = trueIn     n (fmap (== C x) (eval y))
notDiffIn n x y = notFalseIn n (fmap (== C x) (eval y))

lcTestMap :: Map String Test
lcTestMap = let i = (Lam 0 :: Term Nat) in
            fromList [
              ("evalCoterminates",
               Test $ \x n -> let x' = evalN n (x :: Term Nat) in
                              closed x ==> isJust x' || isNothing x'),

              ("omegaDiverges",
               Test $ \n -> isNothing (evalN n (omega :: Term ()))),

              ("eval$$",
               Test $ \x -> trueIn  1 (fmap (== C x) (eval i >>= ($$ x)))),

              ("evalSteps",
               Test $ \x -> equalIn 3 x (i :@ i :@ Const x)),

              ("id",
               Test $ \x -> equalIn 2 x (i :@ Const x)),

              ("yTerminates",
               Test $ \x -> equalIn 7 x ((yComb :: Term Nat) :@ Lam (Lam 0) :@ Const x))
            ]

testsRunner ts n = let next []                  = return ()
                       next ((name, Test t):xs) = do print name
                                                     depthCheck n t
                                                     next xs in
                   next . toList $ ts

testRunner ts n t = case Data.Map.lookup t ts of
                      Nothing       -> error ("Could not find test " ++ t)
                      Just (Test x) -> depthCheck n x

lcTests = testsRunner lcTestMap
lcTest  = testRunner  lcTestMap

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
