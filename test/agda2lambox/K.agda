module _ where

K : {A B : Set} → A → B → A
K = λ a b → a

open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

test : Nat
test = K 42 0
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
