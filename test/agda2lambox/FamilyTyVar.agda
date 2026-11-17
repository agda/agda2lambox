-- type families as type variables
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

record Refined (A : Set) (@0 P : A → Set) : Set where
  constructor _⟨_⟩
  field
    value    : A
    @0 proof : P value
open Refined

refine : (n : Nat) → Refined Nat (λ m → m ≡ n)
refine n = n ⟨ refl ⟩

test : Nat
test = value (refine 42)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
