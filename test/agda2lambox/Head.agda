open import Agda.Builtin.List
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

data NonEmpty {A : Set} : List A → Set where
  instance nel : ∀ {x xs} → NonEmpty (x ∷ xs)

head : ∀ {A} (xs : List A) → @0 ⦃ NonEmpty xs ⦄ → A
head (x ∷ xs) = x

test : Nat
test = head (42 ∷ 0 ∷ [])
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
