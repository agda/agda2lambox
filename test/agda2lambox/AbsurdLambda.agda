open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

data ⊥ : Set where

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A → Maybe A

maybe : {A B : Set} → B → (A → B) → Maybe A → B
maybe b f nothing  = b
maybe b f (just x) = f x

none : Maybe ⊥
none = nothing

count : Maybe (⊥ → Nat) → Nat
count nothing  = 10
count (just _) = 20

apply : (⊥ → Nat) → Maybe ⊥ → Nat
apply k m = maybe 100 k m

capture : Nat → Nat
capture n = maybe n (λ ()) none

test : Nat
test = maybe 1 (λ ()) none + count (just (λ ())) + apply (λ ()) none + capture 1000
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 1121
_ = refl
