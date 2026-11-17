module Records where

open import Haskell.Prim using (Type)
open import Haskell.Prelude using (String; Nat)

variable a b : Type

record Pair (a b : Type) : Type where
  no-eta-equality
  pattern
  constructor MkPair
  field
    proj₁ : a
    proj₂ : b

open Pair public

record Wrap (a : Type) : Type where
  no-eta-equality
  pattern
  field unwrap : a
open Wrap public

record MyMonoid (a : Type) : Type where
  field
    mempty  : a
    mappend : a → a → a

swap : Pair a b → Pair b a
swap (MkPair x y) = MkPair y x

swap₂ : Wrap (Pair a b) → Wrap (Pair b a)
swap₂ (record {unwrap = p}) = record {unwrap = record { proj₁ = proj₂ p; proj₂ = p .proj₁ } }

record User : Type where
  no-eta-equality
  field
    name : String
    code : Nat
open User public

open import Haskell.Prelude using (_+_; []; case_of_; _≡_; refl)

test : Nat
test = proj₁ (swap (MkPair 1 20))
     + proj₂ (unwrap (swap₂ (record { unwrap = MkPair 10 2 })))
     + MyMonoid.mappend m (MyMonoid.mempty m) 5
     + (case name u of λ where
          [] → code u
          _  → 0)
  where
    m : MyMonoid Nat
    m = record { mempty = 0 ; mappend = _+_ }

    u : User
    u = record { name = [] ; code = 7 }
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
