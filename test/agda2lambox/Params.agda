-- ** datatype parameters
--
-- regression test: against issue peregrine-tool#99 (I-84, List used at two types)
--
open import Agda.Builtin.Bool
open import Agda.Builtin.List
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

map : {A B : Set} → (A → B) → List A → List B
map f []       = []
map f (x ∷ xs) = f x ∷ map f xs

isZero : Nat → Bool
isZero zero    = true
isZero (suc _) = false

count : List Bool → Nat
count []           = 0
count (true  ∷ bs) = suc (count bs)
count (false ∷ bs) = count bs

record Pair (A B : Set) : Set where
  constructor _,_
  field fst : A
        snd : B
open Pair

data Fin′ (n : Nat) : Set where
  mk : Nat → Fin′ n

val : ∀ {n} → Fin′ n → Nat
val (mk k) = k

-- mutual datatypes with the same parameters
mutual
  data Tree (A : Set) : Set where
    node : A → Forest A → Tree A

  data Forest A : Set where
    leaf : Forest A
    cons : Tree A → Forest A → Forest A

mutual
  sumTree : Tree Nat → Nat
  sumTree (node x f) = x + sumForest f

  sumForest : Forest Nat → Nat
  sumForest leaf       = 0
  sumForest (cons t f) = sumTree t + sumForest f

-- mutual datatypes with different parameters
mutual
  data Even : Set where
    ez : Even
    es : Odd Nat → Even

  data Odd (A : Set) : Set where
    os : A → Even → Odd A

mutual
  depth : Even → Nat
  depth ez     = 0
  depth (es o) = suc (depthO o)

  depthO : Odd Nat → Nat
  depthO (os n e) = n + depth e

test : Nat
test = count (map isZero (0 ∷ 1 ∷ 0 ∷ []))
     + fst (3 , true) + count (snd (0 , (false ∷ true ∷ [])))
     + val (mk {n = 7} 10)
     + sumTree (node 1 (cons (node 2 leaf) leaf))
     + depth (es (os 5 ez))
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 25
_ = refl
