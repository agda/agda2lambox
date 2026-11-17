open import Agda.Builtin.Nat
open import Agda.Builtin.Equality
open import Agda.Builtin.Sigma

subst : {A : Set} (P : A → Set) {@0 x y : A} → @0 x ≡ y → P x → P y
subst P refl px = px

data Wrap : Set where
  wrap : Nat → Nat → Wrap

inc : @0 Wrap → Nat → Nat
inc (wrap _ _) n = suc n

pairInc : @0 Σ Nat (λ _ → Nat) → Nat → Nat
pairInc (_ , _) n = suc n

record Box (A : Set) : Set where
  no-eta-equality
  pattern
  constructor box
  field unbox : A

boxInc : @0 Box Nat → Nat → Nat
boxInc (box _) n = suc n

splitAfter : Nat → {@0 m : Nat} → @0 m ≡ 3 → Nat
splitAfter zero    refl = 10
splitAfter (suc n) refl = n

splitBefore : {@0 m : Nat} → @0 m ≡ 3 → Nat → Nat
splitBefore refl zero    = 10
splitBefore refl (suc n) = n

data ⊥ : Set where

absurd : {A : Set} → @0 ⊥ → A
absurd ()

safePred : (n : Nat) → @0 (n ≡ 0 → ⊥) → Nat
safePred zero    p = absurd (p refl)
safePred (suc n) _ = n

count : Nat → @0 Wrap → Nat
count zero    (wrap _ _) = 0
count (suc n) w          = suc (count n w)

test : Nat
test = subst (λ _ → Nat) {x = 1} refl 5
     + inc (wrap 1 2) 1
     + pairInc (1 , 2) 1
     + boxInc (box 1) 1
     + splitAfter 4 refl
     + splitBefore refl 0
     + safePred 8 (λ ())
     + count 3 (wrap 0 0)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 34
_ = refl
