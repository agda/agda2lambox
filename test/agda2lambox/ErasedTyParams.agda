open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

twice : ∀ {A : Set} → (A → A) → A → A
twice f a = f (f a)

twice0 : ∀ {@0 A : Set} → (A → A) → A → A
twice0 f a = f (f a)

test : Nat
test = twice suc (twice0 suc 38)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
