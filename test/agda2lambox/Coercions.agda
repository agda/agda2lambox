-- ** coercions along propositional equalities
--
-- `subst` is an identity function, so Agda inlines it and a transport compiles to
-- what it transports; along an index (`pad`) the typed λ□ stays well-typed
--
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

subst : {A : Set} (P : A → Set) {x y : A} → x ≡ y → P x → P y
subst P refl px = px

cong : {A B : Set} (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

+-identityʳ : (n : Nat) → n + 0 ≡ n
+-identityʳ zero    = refl
+-identityʳ (suc n) = cong suc (+-identityʳ n)

infixr 5 _∷_ _++_
data Vec (A : Set) : Nat → Set where
  []  : Vec A 0
  _∷_ : {n : Nat} → A → Vec A n → Vec A (suc n)

_++_ : {A : Set} {m n : Nat} → Vec A m → Vec A n → Vec A (m + n)
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

sum : {n : Nat} → Vec Nat n → Nat
sum []       = 0
sum (x ∷ xs) = x + sum xs

pad : {n : Nat} → Vec Nat n → Vec Nat n
pad {n} xs = subst (Vec Nat) (+-identityʳ n) (xs ++ [])

test : Nat
test = sum (pad (30 ∷ 6 ∷ 6 ∷ []))
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
