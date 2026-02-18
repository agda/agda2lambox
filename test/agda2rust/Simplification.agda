open import Agda.Builtin.Nat using (Nat; _+_)

increment : Nat → Nat
increment = _+ 1

test : Nat
test = increment (increment 40)
{-# COMPILE AGDA2LAMBOX test #-}
-- NB: when the `simplifyTTerm` optimization pass is enabled, we should get `1 + x0`?
