open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A

data Act : Set where
  inc : Act

data Valid : Act → Nat → Set where
  mk : ∀ {α s} → Nat → Valid α s

step : ∀ {α s} → Valid α s → Nat
step {s = s} = λ where (mk n) → s + n

mutual
  data Trace : List Act → Set where
    []    : Trace []
    _∷_⊣_ : ∀ {αs} α (tr : Trace αs) → Valid α (run tr) → Trace (α ∷ αs)

  run : ∀ {αs} → Trace αs → Nat
  run = λ where
    []           → 0
    (_ ∷ _ ⊣ vα) → step vα

test : Nat
test = run (inc ∷ (inc ∷ [] ⊣ mk 1) ⊣ mk 2)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 3
_ = refl
