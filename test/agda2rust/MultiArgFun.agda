open import Agda.Builtin.Nat using (Nat; _+_; _*_; suc)
open import Agda.Builtin.Bool using (Bool; false; true)

exB : Bool → Nat → Nat → Nat
exB false = _*_
exB true  = _+_

exB2 : @0 Nat → Bool → Nat → Nat → Nat
exB2 _ false = _*_
exB2 _ true  = _+_

exB3 : Bool → @0 Nat → Nat → Nat → Nat
exB3 false _ = _*_
exB3 true  _ = _+_

exB4 : Bool → Nat → @0 Nat → Nat → Nat
exB4 false x _ = x *_
exB4 true  x _ = x +_

exB5 : Bool → Nat → Nat → @0 Nat → Nat
exB5 false x y _ = x * y
exB5 true  x y _ = x + y

exH2 : {@0 _ : Nat} → Bool → Nat → Nat → Nat
exH2 false = _*_
exH2 true  = _+_

exH3 : Bool → {@0 _ : Nat} → Nat → Nat → Nat
exH3 false = _*_
exH3 true  = _+_

exH4 : Bool → Nat → {@0 _ : Nat} → Nat → Nat
exH4 false x = x *_
exH4 true  x = x +_

exH5 : Bool → Nat → Nat → {@0 _ : Nat} → Nat
exH5 false x y = x * y
exH5 true  x y = x + y

exF exG : Nat → Nat → Nat
exF x y = x + y
exG = _+_

open import Agda.Builtin.Maybe using (Maybe; just; nothing)

exM : Maybe Nat → Nat → Nat → Nat
exM (just _) = _+_
exM nothing  = _*_

addN : Nat → Nat → Nat
addN n = n +_

apply2 : (Nat → Nat → Nat) → Nat → Nat → Nat
apply2 f x y = f x y

x = apply2 _+_ 40 2
y = apply2 exF 40 2
z = apply2 (λ x y → x + y) 40 2
w = apply2 exG 40 2
q = addN 40 2
r = exG 40 2

open import Agda.Builtin.Nat

test : Nat
test
  = exB true 40 2
  + exB2 10 true 40 2
  + exB3 true 10 40 2
  + exB4 false 21 10 2
  + exB5 true 2 40 10

  + exH2 {10} true 40 2
  + exH3 true {10} 40 2
  + exH4 true 40 {10} 2
  + exH5 true 40 2 {10}

  + x
  + y
  + z
  + w
  + q
  + r
{-# COMPILE AGDA2LAMBOX test #-}
