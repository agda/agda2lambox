import Haskell.Prelude as Pre

_+_ : Pre.Nat → Pre.Nat → Pre.Nat
x + y = x

comp : (Pre.Nat → Pre.Nat) → (Pre.Nat → Pre.Nat) → (Pre.Nat → Pre.Nat)
comp f g = f Pre.∘ g

test₀ : Pre.Nat
test₀ = 0 Pre.+ 1 + 0

testComp : Pre.Nat
testComp = comp (_+ 0) (Pre._+ 1) 0

testFoldr : Pre.List Pre.Nat → Pre.Nat
testFoldr = Pre.foldr (λ _ x → x) 0

import Haskell.Prelude as pre

retest : pre.Nat
retest = 0 pre.+ 1 + 0

open import Haskell.Prelude using (_≡_; refl)

test : Pre.Nat
test = test₀ Pre.+ testComp Pre.+ testFoldr (1 Pre.∷ 2 Pre.∷ Pre.[]) Pre.+ retest
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 3
_ = refl
