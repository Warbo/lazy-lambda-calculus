module Data.Language.LC.Self where

import Debug.Trace

import Control.Monad.Partial
import Data.Language.LC
import Data.Nat
import Data.Typeable

-- Lambda calculus inside lambda calculus, using Morgensen-Scott encoding

decode :: (Encodable a) => Term a -> Partial (Val a)
decode = (>>= unmse) . eval

extractC :: Partial (Val a) -> Partial (Maybe a)
extractC = let f (C x) = Just x
               f  _    = Nothing in
           fmap f

class (Typeable a) => Encodable a where
  mse   :: a -> Term  b
  unmse :: (Typeable b) => Val b -> Partial (Val a)

instance Encodable () where
  mse  () = Lam 0
  unmse x = cast' x >>= ($$ ())

instance Encodable Bool where
  mse True  = Lam (Lam 1)
  mse False = Lam (Lam 0)
  unmse x = cast' x >>= ($$ True) >>= ($$ False)

instance Encodable Nat where
  mse x = let wrap Z = 1
              wrap (S n) = 0 :@ wrap n in
          Lam (Lam (wrap x))

  unmse x = (eval' (0 :@ 1 :@ 2) [cast' x, Now (C 0), Now (F (fmap (valMap S)))])
{-
instance (Encodable a, Show a) => Encodable (Partial a) where
  mse (Now   x) = Lam (Lam (1 :@ mse x))
  mse (Later x) = Lam (Lam (0 :@ mse x))

  unmse x = eval' (0 :@ 1 :@ 2) [Now x,
                                 Now (F (\y -> do y'    <- y
                                                  C y'' <- unmse y'
                                                  return (C y''))),
                                 Now (F (fmap (valMap Later)))]

instance Encodable a => Encodable [a] where
  mse []     = Lam (Lam 1)
  mse (x:xs) = Lam (Lam (0 :@ mse x :@ mse xs))

instance Encodable a  => Encodable (Term a) where
  mse (Const x) = Lam (Lam (Lam (Lam (3 :@ mse x))))
  mse (Var   n) = Lam (Lam (Lam (Lam (2 :@ mse n))))
  mse (Lam   f) = Lam (Lam (Lam (Lam (1 :@ mse f))))
  mse (l :@  r) = Lam (Lam (Lam (Lam (0 :@ mse l :@ mse r))))

{-
Lookup an element in a List. We can ignore empty lists:

selfLookup (h:t)  Z    = h
selfLookup (h:t) (S n) = selfLookup t n

We can abstract out the recursion using a Z combinator:

selfLookup = let f g (h:t)  Z    = h
                 f g (h:t) (S n) = g t n in
             zComb f

By definition '(h:t) A B = B h t', which eliminates the pattern-match:

selfLookup = let f g l  Z    = l id (\a b -> a)
                 f g l (S n) = l id (\a b -> g b n) in
             zComb f

By definition 'Z A B = A' and '(S n) A B = B n', which eliminates the remaining
pattern-match:

selfLookup = let f g l m = m (l id (\a b -> a))
                             (\n -> l id (\a b -> g b n)) in
             zComb f

Now we can put everything inline:

selfLookup = zComb (\g l m -> m (l id (\a b -> a))
                                (\n -> l id (\a b -> g b n)))

-}
selfLookup = let z = 1 :@ mse () :@ mse Z
                 s = Lam (2 :@ mse () :@ Lam (Lam (5 :@ 0 :@ 2))) in
             zComb :@ Lam (Lam (Lam (0 :@ z :@ s)))
-}
