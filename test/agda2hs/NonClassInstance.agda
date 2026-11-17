open import Haskell.Prelude
open import Haskell.Extra.Dec
open import Haskell.Extra.Refinement

foo : (b : Bool) → {{Dec (IsTrue b)}} → Bool
foo _ {{b ⟨ _ ⟩}} = not b

bar : Bool → Bool
bar b = foo b

test : Nat
test = (if bar True then 0 else 21) + (if bar False then 21 else 0)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
