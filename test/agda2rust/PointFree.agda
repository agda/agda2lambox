-- Issue #2
open import Agda.Builtin.Nat using (suc) renaming (Nat to ℕ)

data Foo : Set where
  foo : ℕ → Foo

f : ℕ → Foo
f = foo

g : ℕ → ℕ
g = suc

h : ℕ → ℕ
h = g

open import Agda.Builtin.Nat

toNat : Foo → Nat
toNat (foo n) = n

test : Nat
test = toNat (f 42)
     + g 41
     + h 41
{-# COMPILE AGDA2LAMBOX test #-}
