module SelfProjection where

open import Agda.Builtin.Bool
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

data ⊥ : Set where

if_then_else_ : {A : Set₁} → Bool → A → A → A
if true  then t else _ = t
if false then _ else e = e

data Reflects (A : Set) : Bool → Set where
  ofʸ : A → Reflects A true
  ofⁿ : (A → ⊥) → Reflects A false

of : ∀ {A b} → (if b then A else (A → ⊥)) → Reflects A b
of {b = false} ¬a = ofⁿ ¬a
of {b = true}  a  = ofʸ a

invert : ∀ {A b} → Reflects A b → if b then A else (A → ⊥)
invert (ofʸ a)  = a
invert (ofⁿ ¬a) = ¬a

record Dec (A : Set) : Set where
  constructor _because_
  field does  : Bool
        proof : Reflects A does
open Dec

map′ : ∀ {A B} → (A → B) → (B → A) → Dec A → Dec B
does  (map′ f g a?)                 = does a?
proof (map′ f g (true  because  p)) = of (f (invert p))
proof (map′ f g (false because ¬p)) = of (λ b → invert ¬p (g b))
-- ^ the implicit argument of `of` is `does (map′ f g (_ because _))`,
--   which loops under strict evaluation unless unfolded to `true`/`false`.

suc-inj : ∀ {m n} → suc m ≡ suc n → m ≡ n
suc-inj refl = refl

cong-suc : ∀ {m n} → m ≡ n → suc m ≡ suc n
cong-suc refl = refl

_≟_ : (m n : Nat) → Dec (m ≡ n)
zero  ≟ zero  = true because ofʸ refl
zero  ≟ suc n = false because ofⁿ λ ()
suc m ≟ zero  = false because ofⁿ λ ()
suc m ≟ suc n = map′ cong-suc suc-inj (m ≟ n)

test : Bool
test = does (3 ≟ 3)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ true
_ = refl
