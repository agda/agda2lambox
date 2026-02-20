open import Agda.Builtin.Nat

test : Nat
test = 1 + 2
{-# COMPILE AGDA2LAMBOX test #-}
