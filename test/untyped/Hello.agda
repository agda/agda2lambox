open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.Equality

String : Set
String = List Nat

test : String
test = 72 ∷ 101 ∷ 108 ∷ 108 ∷ 111 ∷ 33 ∷ []
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 72 ∷ 101 ∷ 108 ∷ 108 ∷ 111 ∷ 33 ∷ []
_ = refl
