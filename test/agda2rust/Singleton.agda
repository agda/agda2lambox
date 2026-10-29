open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

data The {ℓ}{A : Set ℓ} : A → Set ℓ where
  the : (a : A) → The a

_ : The 42
_ = the 42

toNat : ∀ {n : Nat} → The n → Nat
toNat {n} _ = n

test : Nat
test = toNat (the 42)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
