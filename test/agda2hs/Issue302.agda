open import Haskell.Prelude

not0 : Integer → Bool
not0 n = n /= 0

test : Nat
test = (if not0 1 then 21 else 0) + (if not0 0 then 0 else 21)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
