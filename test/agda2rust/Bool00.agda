data Bool : Set where
  false true : Bool

_∧_ : Bool → Bool → Bool
_∧_ = λ where
  true true → true
  _    _    → false

-- make sure reserved Rust names do not conflict
r#true  = true
r#false = false

testBool : Bool
testBool = r#true ∧ r#false

open import Agda.Builtin.Nat using (Nat; zero; suc)

bool2Nat : Bool → Nat
bool2Nat = λ where
  true  → 0
  false → 42

-- Issue #3
isZero : Nat → Bool
isZero zero    = true
isZero (suc _) = false

toNonZero : Nat → Nat
toNonZero x with isZero x
... | true  = 1
... | false = x

if_then_else_ : ∀ {A : Set} → Bool → A → A → A
if true  then t else _ = t
if false then _ else f = f

testIte : Nat
testIte = if true then 42 else 0

open import Agda.Builtin.Nat

test : Nat
test = bool2Nat testBool
     + bool2Nat (isZero 5)
     + toNonZero 42
     + testIte
{-# COMPILE AGDA2LAMBOX test #-}
