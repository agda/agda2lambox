module TypeOperators where

open import Haskell.Prim using (Type)
open import Haskell.Prim.Either

open import Agda.Builtin.Nat
open import Agda.Builtin.Bool

_:++:_ : Type → Type → Type
_:++:_ = Either

mx : Bool :++: Nat
mx = Left true

_++++_ : Type → Type → Type
_++++_ = Either

mx' : Bool ++++ Nat
mx' = Left true

data _****_ (a b : Type): Type where
  _:****_ : a → b → a **** b

cross : Bool **** Nat
cross = true :**** 1

open import Agda.Builtin.Equality

test : Nat
test = fromE mx + fromE mx' + both cross
  where
    fromE : Bool :++: Nat → Nat
    fromE (Left true)  = 20
    fromE (Left false) = 0
    fromE (Right n)    = n

    both : Bool **** Nat → Nat
    both (true  :**** n) = suc n
    both (false :**** _) = 0
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
