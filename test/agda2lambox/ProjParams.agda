-- restoring projection-like parameters in typed λ□
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

record Number (A : Set) : Set₁ where
  field
    Constraint : Nat → Set
    fromNat    : (n : Nat) → Constraint n → A
open Number

-- on a variable: `[A] |- Constraint A number 0`
record Num (A : Set) : Set₁ where
  field
    number : Number A
    zeroOK : number .Constraint 0
open Num

numberNat : Number Nat
numberNat = record { Constraint = λ _ → Nat ; fromNat = λ n k → n + k }

-- on a known record: `[] |- Constraint Nat numberNat 0`, not `[A:=Nat] |-..`
two : numberNat .Constraint 0
two = 2

numNat : Num Nat
numNat = record { number = numberNat ; zeroOK = two }

fromZero : {A : Set} → Num A → A
fromZero d = d .number .fromNat 0 (d .zeroOK)

test : Nat
test = fromZero numNat + 40
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
