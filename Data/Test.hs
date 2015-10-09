{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleContexts #-}
{-# Language ExistentialQuantification #-}

module Data.Test where

import Control.Monad.Partial
import Data.Nat
import Data.Data
import Data.Language.LC
import Data.Map
import Data.Maybe
import Data.Typeable
import Test.LazySmallCheck2012 hiding (Nat, Term, Const)
import Test.LazySmallCheck2012.Core hiding (Term)

data Test = forall a. (Typeable a,
                       Data a,
                       Testable a) => Test a

runDepthCheck n (Test t) = depthCheck n t
