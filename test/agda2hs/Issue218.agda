module Issue218 where

open import Haskell.Prelude
open import Haskell.Extra.Erase
open import Haskell.Extra.Refinement

module _ (@0 n : Integer) where

  foo : {{Singleton n}} → ∃ Integer (_≡ n)
  foo {{sing n}} = n ⟨ refl ⟩

bar : ∃ Integer (_≡ 42)
bar = foo _

test : Integer
test = value bar
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
