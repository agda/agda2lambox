{-# OPTIONS --erasure #-}
-- compilation of λ□ type schemes
module Scheme where

open import Agda.Builtin.List
open import Agda.Builtin.Nat
open import Agda.Builtin.Bool
open import Agda.Builtin.Equality

length : {A : Set} → List A → Nat
length [] = 0
length (_ ∷ xs) = suc (length xs)



record Σ (A : Set) (@0 B : A → Set) : Set where
  constructor _,_
  field
    fst    : A
    @0 snd : B fst
Σ-syntax : (A : Set) (@0 B : A → Set) → Set
Σ-syntax = Σ

syntax Σ-syntax A (λ x → B) = [ x ∈ A ∣ B ]

-- expected: type scheme
-- ­ vars: [a, b]
-- ­ type: a → b
Arrow : (A B : Set) → Set
Arrow A B = A → B

-- expected: type scheme
-- - vars: [a]
-- - type: List a
ListAlias : Set → Set
ListAlias = List

ListAlias' : Set → Set
ListAlias' A = List A

-- Rocq-like vectors --

-- expected: type scheme
-- - vars: [a, n]
-- - type: Σ (List a) □
Vec : (A : Set) → Nat → Set
Vec A n = [ xs ∈ List A ∣ length xs ≡ n ]

Bad : Bool → Set
Bad false = Nat
Bad true  = Bool 
