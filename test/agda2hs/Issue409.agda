open import Haskell.Prelude

{-# NO_POSITIVITY_CHECK #-}
record SUL (sul : Set → Set → Set) : Set₂ where
  field
    step  : ∀ {i o} → sul i o → i → (sul i o × o)
    reset : ∀ {i o} → sul i o → sul i o

open SUL {{...}}

record Automaton (aut : Set → Set → Set) (s : Set) : Set₂ where
  field
    overlap ⦃ super ⦄ : SUL aut
    current : ∀ {i o} → aut i o → s

test : Nat
test = case step ⦃ Automaton.super aut ⦄ (0 , id) 41 of λ where
    (s , o) → Automaton.current aut (reset s) + Automaton.current aut s + o
  where
    M : Set → Set → Set
    M i o = Nat × (i → o)

    instance
      iSUL : SUL M
      iSUL .step (n , f) x = ((suc n , f) , f x)
      iSUL .reset (_ , f)  = (0 , f)

    aut : Automaton M Nat
    aut .Automaton.super   = iSUL
    aut .Automaton.current = fst
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
