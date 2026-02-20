module _ where

K : {A B : Set} → A → B → A
K = λ a b → a

open import Agda.Builtin.Nat

test : Nat
test = K 42 0
{-# COMPILE AGDA2LAMBOX test #-}
