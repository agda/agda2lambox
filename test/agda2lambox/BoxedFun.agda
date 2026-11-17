-- ** erased arguments of a function type
--
-- regression test: Agda erases `toTT`, as its type `Nat → ⊤` is erasable,
-- but `both` takes it at the type `A → B`, so typed λ□ passes `λ _ → □`
--
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

record ⊤ : Set where
  constructor tt

toTT : Nat → ⊤
toTT _ = tt

both : {A B : Set} → (A → B) → (B → A) → A → A
both f g x = g (f x)

test : Nat
test = both toTT (λ _ → 42) 0
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
