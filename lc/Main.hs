module Main where

import Test.LazySmallCheck2012
import Control.Applicative
import Data.Language.LC.Test
import Data.Language.LC.Self.Test

-- FIXME: Make this a test suite rather than an executable (e.g. using tasty)

--allTests = print <|> lcTests <|> selfTests

--main = sequence . map allTests $ [0..]
main = depthCheck 6 decodeNat
