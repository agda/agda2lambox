data Unit : Set where
  tt : Unit

idUnit : Unit → Unit
idUnit tt = tt

open import Agda.Builtin.Nat

toNat : Unit → Nat
toNat _ = 42

test : Nat
test = toNat (idUnit tt)
{-# COMPILE AGDA2LAMBOX test #-}
