open import Haskell.Prelude

testMax : (x y : Nat) → max (suc x) (suc y) ≡ suc (max x y)
testMax x y = refl

testMin : (x y : Nat) → min (suc x) (suc y) ≡ suc (min x y)
testMin x y = refl

test : Nat
test = max 2 40 + min 2 40
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
