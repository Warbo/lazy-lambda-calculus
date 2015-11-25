module Main where

import Test.LazySmallCheck2012
import Control.Applicative
import Data.Language.LC.Test
import Data.Language.LC.Self.Test

--allTests = print <|> lcTests <|> selfTests

--main = sequence . map allTests $ [0..]
main = depthCheck 6 decodeNat
