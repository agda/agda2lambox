open import Agda.Builtin.Bool using (Bool; true; false)

tt = true

not : Bool → Bool
not true  = false
not false = true

_∧_ : Bool → Bool → Bool
_∧_ = λ where
  true true → true
  _    _    → false

testBool : Bool
testBool = true ∧ false

open import Agda.Builtin.Nat using (Nat)

bool2Nat : Bool → Nat
bool2Nat = λ where
  true  → 0
  false → 42

if_then_else_ : ∀ {A : Set} → Bool → A → A → A
if true  then t else _ = t
if false then _ else f = f

testIte : Nat
testIte = if true then 42 else 0

open import Agda.Builtin.Nat

test : Nat
test = bool2Nat testBool
     + testIte
{-# COMPILE AGDA2LAMBOX test #-}
