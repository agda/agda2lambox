module EtaCon where

open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.Equality

addone : Nat → Nat
addone = suc

cons : {A : Set} → A → List A → List A
cons = _∷_

test : List Nat
test = cons 1 []
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 1 ∷ []
_ = refl
