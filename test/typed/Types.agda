{-# OPTIONS --prop #-}
-- type translation tests, from the paper
-- "Extracting functional programs from Coq, in Coq"

open import Agda.Builtin.Nat
open import Agda.Builtin.List

listrect
  : {A : Set} (P : List A → Set)
  → P []
  → (∀ x xs → P xs → P (x ∷ xs))
  → ∀ xs → P xs
listrect P pnil pcons [] = pnil
listrect P pnil pcons (x ∷ xs) = pcons x xs (listrect P pnil pcons xs)

map : {A B : Set} → (A → B) → List A → List B
map f = listrect _ [] λ x _ ys → f x ∷ ys

head : List Nat → Nat
head [] = 0
head (n ∷ _) = n

record sig (A : Set) (P : A → Prop) : Set where
  constructor exist
  field
    fst : A
    snd : P fst

data Top : Prop where
  tt : Top

test : Nat
test = head (map (λ (x : sig Nat (λ _ → Top)) → suc (x .sig.fst))
                 (exist 41 tt ∷ exist 0 tt ∷ []))
{-# COMPILE AGDA2LAMBOX test #-}
