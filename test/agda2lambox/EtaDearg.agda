-- dearging partial applications
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

data Box (A : Set) : Set where
  pack : A → Box A

unpack : {A : Set} → Box A → A
unpack (pack x) = x

record ⊤ : Set where
  constructor tt

-- the continuation leaves out the dead type argument of `applyPlus`
applyPlus : Nat → {A : Set} → (A → Nat) → A → Nat
applyPlus m f x = m + f x

-- rank-2, so used at a type that Rust also sees as `()`
withPoly : ({A : Set} → (A → Nat) → A → Nat) → Nat
withPoly k = k {⊤} (λ _ → 20) tt

-- the continuation leaves out the dead erased argument of `addBoth`
addBoth : Nat → Nat → @0 Nat → Box Nat
addBoth m n _ = pack (m + n)

withTwo : (Nat → @0 Nat → Box Nat) → Box Nat
withTwo k = k 2 0

test : Nat
test = withPoly (applyPlus 1) + unpack (withTwo (addBoth 19))
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
