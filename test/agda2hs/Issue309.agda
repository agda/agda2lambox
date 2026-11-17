module Issue309 where

open import Haskell.Prim using (Type)

private variable @0 a : Type

Ap : (p : @0 a → Type) → @0 a → Type
Ap p x = p x

open import Agda.Builtin.Nat using (Nat)
open import Agda.Builtin.Equality

test : Ap (λ _ → Nat) 0
test = 42
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
