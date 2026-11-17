-- ** families given as definitions
--
-- regression test: a family argument that is not a λ (`_≡_`) is η-expanded, and
-- compiles to the type of its body (□), since that does not depend on the binders;
-- the field `refl′`, which Agda erases, is then a function returning □
--
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

record Refl (A : Set) (R : A → A → Set) : Set where
  constructor mk
  field
    witness : A
    refl′   : (x : A) → R x x
open Refl

reflNat : Refl Nat _≡_
reflNat = mk 42 (λ _ → refl)

get : ∀ {A R} → Refl A R → A
get r = witness r

test : Nat
test = get reflNat
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
