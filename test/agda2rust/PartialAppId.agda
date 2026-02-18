open import Agda.Builtin.Nat using (Nat; _+_)

id : ∀ {A : Set} → A → A
id x = x

idd : ∀ {A : Set} → (A → A) → A → A
idd id x = id x

testIdd : Nat
testIdd = idd id 42

testIdId : Nat
testIdId = (id id) 42
{- id(id)(42) -}

testIdIdId : Nat
testIdIdId = id id id 42
{- id(id)(id)(42) -}
{- id(λ x → id x)(λ x → id x)(42) -}

testNestedId : Nat
testNestedId = id (id id) (id id id) (id id 42)
{- id(id(id))(id(id)(id))(id(id)(42)) -}

testIdAdd : Nat
testIdAdd = id _+_ 40 2
{-
id : ∀ {A} → A → A
id _+_ : Nat → Nat → Nat (A = Nat → Nat → Nat)
id _+_ 40 : Nat → Nat
id _+_ 40 2 : Nat
-}
{- id(_+_, 40, 2) -}
{- id(_+_)(40, 2) -}

add3 : Nat → Nat → Nat → Nat
add3 x y z = x + y + z

testIdAdd3 : Nat
testIdAdd3 = id add3 40 1 1
{- id(add3)(40, 1, 1) -}
{- id(λ x y z → add3 x y z)(40, 1, 1) -}

idF : (Nat → Nat → Nat) → Nat
idF f = id f 40 2
{- id(f)(40, 2) -}
{- id(λ x y → f x y)(40, 2) -}

testIdF : Nat
testIdF = idF _+_

idF3 : (Nat → Nat → Nat → Nat) → Nat
idF3 f = id f 40 1 1
{- id(f)(40, 1, 1) -}
{- id(λ x y z → f x y z)(40, 1, 1) -}

testIdF3 : Nat
testIdF3 = idF3 add3

open import Agda.Builtin.Nat

test : Nat
test
  = testIdd
  + testIdId
  + testIdIdId
  + testNestedId

  + testIdAdd
  + testIdAdd3
  + testIdF
  + testIdF3
{-# COMPILE AGDA2LAMBOX test #-}
