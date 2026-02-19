open import Agda.Builtin.Bool
open import Agda.Builtin.Nat

Ret : Bool → Set
Ret false = Bool
Ret true  = Nat

not : Bool → Bool
not false = true
not true = false

f : (b : Bool) → Ret b → Ret b
f false x = not x
f true  x = suc x

test : Nat
test = f true 5
{-# COMPILE AGDA2LAMBOX test #-}
