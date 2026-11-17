open import Haskell.Prelude hiding (f)

record Test (a : Type) : Type₁ where
  field
    f : a -> a
open Test {{...}} public

instance
  testNat : Test Nat
  Test.f testNat n = h
    where
    g : Nat
    g = 3 + n
    h : Nat
    h = n + g

f1 : Nat -> Nat
f1 n = h1
  where
  g1 : Nat
  g1 = 3 + n
  h1 : Nat
  h1 = n + g1

f2 : Nat -> Nat
f2 n = h2 n
  where
  g2 : Nat
  g2 = 3 + n
  h2 : Nat -> Nat
  h2 m = n + g2 + m

test : Nat
test = f 4 + f1 5 + f2 5
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
