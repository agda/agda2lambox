-- {-# OPTIONS --exact-split #-}
open import Agda.Builtin.Nat

-- non-exact split generates "unreachable pattern" warning
min : Nat → Nat → Nat
min zero    _       = zero
min _       zero    = zero
-- min (suc _) zero    = zero
min (suc n) (suc m) = min n m

test : Nat
test = min zero zero
{-# COMPILE AGDA2LAMBOX test #-}
