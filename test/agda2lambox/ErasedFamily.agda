-- ** type families with erased values
--
-- Agda erases the values of a stuck type-level function whose clauses are all erased,
-- so typed λ□ types them □ rather than with the family, which has no λ□ definition.
open import Agda.Builtin.Nat
open import Agda.Builtin.Bool
open import Agda.Builtin.List
open import Agda.Builtin.Equality

record ⊤ : Set where
  constructor tt

record Pair : Set where
  constructor _,_
  field fst snd : ⊤

length : {A : Set} → List A → Nat
length []       = 0
length (_ ∷ xs) = suc (length xs)

F : Bool → Set
F true  = ⊤
F false = Pair

useF : (b : Bool) → F b → Nat
useF b x = length (x ∷ x ∷ [])

G : Nat → Set
G zero    = ⊤
G (suc n) = G n

useG : (n : Nat) → G n → Nat
useG n x = length (x ∷ [])

test : Nat
test = useF true tt + useF false (tt , tt) + useG 3 tt
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 5
_ = refl
