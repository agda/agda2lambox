open import Haskell.Prim using (Type)
open import Haskell.Prelude using ( Integer ; fst ; snd
                                  ; a ; b
                                  ; _×_ ; _,_
                                  ; _≡_; refl
                                  ; List; map
                                  )

data Indexed (a : Type) : Type where
    MkIndexed : Integer × a → Indexed a

index : Integer × a → Indexed a
index = MkIndexed

data Pair (a b : Type) : Type where
    MkPair : a × b → Pair a b

record Identity (a : Type) : Type where
    no-eta-equality
    constructor MkIdentity
    field
        runIdentity : a
open Identity public

record Equal (a : Type) : Type where
    no-eta-equality
    constructor MkEqual
    field
        pair : a × a
        @0 proof : fst pair ≡ snd pair
open Equal public

record Duo (a : Type) : Type where
    no-eta-equality
    field
        duo : a × a
open Duo public

createDuo : a → a → Duo a
createDuo a b = record { duo = a , b }

open import Haskell.Prelude using (Nat; _+_; case_of_; fromNat)

test : Nat
test = snd (case index (1 , 21) of λ where (MkIndexed p) → p)
     + (case MkPair (2 , 3) of λ where (MkPair p) → fst p + snd p)
     + runIdentity (MkIdentity 4)
     + fst (pair (MkEqual (5 , 5) refl))
     + snd (duo (createDuo 6 7))
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
