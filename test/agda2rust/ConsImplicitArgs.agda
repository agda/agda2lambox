open import Agda.Builtin.Nat using (Nat)

data Bar : Set where
  bar : Bar

data Foo : Set where
  foo : {Bar} → Foo

fooToNat : Foo → Nat
fooToNat (foo {bar}) = 42

testFoo : Foo
testFoo = foo {bar}

-- Polymorphic variant.
data BarA {ℓ} (A : Set ℓ) : Set ℓ where
  bar : A → BarA A

data FooA {ℓ} (A : Set ℓ) : Set ℓ where
  foo : {BarA A} → FooA A

fooAToNat : FooA Nat → Nat
fooAToNat (foo {bar n}) = n

testFooA : FooA Nat
testFooA = foo {_}{_}{bar 42}

-- Record variant.

record BarR : Set where
  constructor bar
open BarR

record FooR : Set where
  constructor foo
  field {getBar} : BarR
open FooR

fooRToNat : FooR → Nat
fooRToNat (foo {bar}) = 42

testFooR : FooR
testFooR = foo {bar}

-- Polymorphic record variant.

record BarRA {ℓ} (A : Set ℓ) : Set ℓ where
  constructor bar
open BarRA

record FooRA {ℓ} (A : Set ℓ) : Set ℓ where
  constructor foo
  field {getBar} : BarRA A
open FooRA

fooRAToNat : FooRA Nat → Nat
fooRAToNat (foo {bar}) = 42

testFooRA : FooRA Nat
testFooRA = foo {_}{_}{bar}

open import Agda.Builtin.Nat

test : Nat
test = fooToNat testFoo
     + fooAToNat testFooA
     + fooRToNat testFooR
     + fooRAToNat testFooRA
{-# COMPILE AGDA2LAMBOX test #-}
