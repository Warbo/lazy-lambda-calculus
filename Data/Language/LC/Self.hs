module Data.Language.LC.Self where

import Control.Monad.Partial
import Data.Language.LC
import Data.Nat

-- Lambda calculus inside lambda calculus, using Morgensen-Scott encoding

class Encodable a where
  mse :: a -> Term b
  decode :: Term a -> Partial (Val a)

instance Encodable () where
  mse () = Lam 0
  decode x = eval (x :@ Const ())

instance Encodable Bool where
  mse True  = Lam (Lam 1)
  mse False = Lam (Lam 0)
  decode x = eval (x :@ Const True :@ Const False)

instance Encodable Nat where
  mse x = let wrap Z = 1
              wrap (S n) = 0 :@ wrap n in
          Lam (Lam (wrap x))
{-
  decode x = let s :: Partial Val Nat -> Partial Val Nat
                 s = fmap s'
                 s' :: Val Nat -> Val Nat
                 s' (C n) = C (S n)
                 s' (F f) = F (F (fmap f)) -- f :: Partial Val Nat -> Partial Val Nat
                 s y = do y' <- y
                          return (case y' of
                                    C y'' -> C (S y'')
                                    F f   -> F (s . f)) in
             eval (x :@ Const Z) >>= ($$ F s)
-}
instance Encodable a => Encodable (Partial a) where
  mse (Now   x) = Lam (Lam (1 :@ mse x))
  mse (Later x) = Lam (Lam (0 :@ mse x))

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
