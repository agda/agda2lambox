open import Agda.Builtin.Bool
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

odd even : Nat → Bool

odd zero    = false
odd (suc n) = even n

even zero    = true
even (suc n) = odd n

test : Bool
test = odd 1
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ true
_ = refl
