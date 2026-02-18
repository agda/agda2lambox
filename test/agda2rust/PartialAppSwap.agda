open import Agda.Builtin.Nat using (Nat; _+_)

swapF : ∀ {A B : Set} → (A → A → B) → (A → A → B)
swapF f x y = f y x

testSwapAdd : Nat
testSwapAdd = swapF _+_ 40 2

testSwapAdd2 : Nat → Nat
testSwapAdd2 x = swapF _+_ x 2

testSwapSwap : Nat
testSwapSwap = swapF (swapF _+_) 40 2

add3 : Nat → Nat → (Nat → Nat)
add3 x y = λ z → x + y + z

testSwapAdd3 : Nat
testSwapAdd3 = swapF add3 40 1 1

testSwapSwap3 : Nat
testSwapSwap3 = swapF (swapF add3) 40 1 1

open import Agda.Builtin.Nat

test : Nat
test
  = testSwapAdd
  + testSwapAdd2 40
  + testSwapSwap
  + testSwapAdd3
  + testSwapSwap3
{-# COMPILE AGDA2LAMBOX test #-}
