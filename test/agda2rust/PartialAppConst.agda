open import Agda.Builtin.Nat using (Nat; _+_)

const : ∀ {A : Set} → A → A → A
const x _ = x

add3 add2 : Nat → Nat → Nat → Nat
add3 x y z = x + y + z
add2 x y _ = x + y

testConst : Nat
testConst = const add3 add2 40 1 1

data Bool : Set where
  T F : Bool

if_then_else_ : ∀ {A : Set} → Bool → A → A → A
if T then t else _ = t
if F then _ else f = f

testIte : Nat
testIte = (if T then add3 else add2) 40 1 1

open import Agda.Builtin.Nat

test : Nat
test = testConst
     + testIte
{-# COMPILE AGDA2LAMBOX test #-}
