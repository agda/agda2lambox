-- ** coercions between types
--
-- a transport along an equality of types compiles to what it transports, so typed λ□
-- loses the cast and `x : A` lands in a `Vec B`, which rustc and typed Koka reject
--
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

subst : ∀ {a} {A : Set a} (P : A → Set) {x y : A} → x ≡ y → P x → P y
subst P refl px = px

infixr 5 _∷_
data Vec (A : Set) : Nat → Set where
  []  : Vec A 0
  _∷_ : {n : Nat} → A → Vec A n → Vec A (suc n)

sum : {n : Nat} → Vec Nat n → Nat
sum []       = 0
sum (x ∷ xs) = x + sum xs

coeAll : {A B : Set} {n : Nat} → A ≡ B → Vec A n → Vec B n
coeAll eq []       = []
coeAll eq (x ∷ xs) = subst (λ X → X) eq x ∷ coeAll eq xs

test : Nat
test = sum (coeAll refl (30 ∷ 6 ∷ 6 ∷ []))
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
