open import Haskell.Prelude

{-# NO_POSITIVITY_CHECK #-}
record SUL (sul : Set → Set → Set) : Set₂ where
  field
    step  : ∀ {i o} → sul i o → i → (sul i o × o)
    reset : ∀ {i o} → sul i o → sul i o

open SUL {{...}}  -- open fields from instance

-- Automaton class, with `current` method only for now
record Automaton (aut : Set → Set → Set → Set) : Set₃ where
  field
    overlap ⦃ super ⦄ : ∀ {s} → SUL (aut s)
    current : ∀ {s i o} → aut s i o → s

open Automaton {{...}}

test : Nat
test = snd (step ⦃ iSUL ⦄ (reset ⦃ iSUL ⦄ suc) 20)
     + current ⦃ iAut ⦄ (reset ⦃ Automaton.super iAut ⦄ (21 , suc))
  where
    iSUL : SUL (λ i o → i → o)
    iSUL = record { step = λ m x → m , m x ; reset = id }

    iAut : Automaton (λ s i o → s × (i → o))
    iAut = record { super = record { step = λ p x → p , snd p x ; reset = id } ; current = fst }
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
