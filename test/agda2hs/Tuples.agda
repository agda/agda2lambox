module Tuples where

open import Haskell.Prelude

swap : a × b → b × a
swap (a , b) = b , a

data TuplePos : Type where
  Test : TuplePos × Bool → TuplePos

t1 : Bool × Bool × Bool
t1 = True , False , True

t2 : (Bool × Bool) × Bool
t2 = (True , False) , True

t3 : Bool × (Bool × Bool)
t3 = True , (False , True)

pair : Integer × Integer
pair = 1 , 2

test₀ : Integer
test₀ = let (x , y) = pair in x + y

test2 : Bool
test2 = case t1 of \where
  (a , b , c) → c

open import Haskell.Extra.Sigma as S using (Σ-syntax)
open import Haskell.Extra.Dec
open import Haskell.Prim using (itsTrue)
open import Haskell.Extra.Refinement

t4 : Σ[ n ∈ Nat ] (Dec (IsTrue (n <= 5)))
t4 = 3 S., (True ⟨ itsTrue ⟩)

t5 : Σ[ x ∈ a ] b → a
t5 p = case p of λ where (x S., y) → x

test : Integer
test = test₀
     + (if test2 then 1 else 0)
     + (if fst (fst t2) then 1 else 0)
     + (if snd (swap t3) then 1 else 0)
     + (case t4 of λ where
          (_ S., (b ⟨ _ ⟩)) → if b then 1 else 0)
     + t5 (1 S., True)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 8
_ = refl
