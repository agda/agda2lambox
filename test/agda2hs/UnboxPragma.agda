open import Haskell.Prelude

record ∃ (A : Type) (@0 P : A → Type) : Type where
  constructor _[_]
  field
    el : A
    @0 pf : P el
open ∃ public

postulate
  IsSorted : List Integer → Type
  looksfine : {xs : List Integer} → IsSorted xs

sort1 : List Integer → ∃ (List Integer) IsSorted
sort1 xs = xs [ looksfine ]

sort2 : List Integer → ∃ (List Integer) IsSorted
sort2 xs .el = xs
sort2 xs .pf = looksfine

sort3 : List Integer → ∃ (List Integer) IsSorted
sort3 xs = xs [ sort2 xs .pf ]

sortAll : List (List Integer)
sortAll = map el (map (λ xs → xs [ looksfine {xs} ]) ((1 ∷ 2 ∷ []) ∷ (3 ∷ []) ∷ []))

record Σ0 (A : Type) (P : @0 A → Type) : Type where
  constructor _[_]
  field
    @0 el : A
    pf : P el
open Σ0 public

Scope : (name : Type) → Type
Scope name = Σ0 (List name) λ xs → ∃ Integer λ n → intToInteger (length xs) ≡ n

emptyScope : {name : Type} → Scope name
emptyScope = [] [ 0 [ refl ] ]

test : Integer
test = total (concat sortAll)
     + total (∃.el (sort1 (10 ∷ [])))
     + total (∃.el (sort2 (12 ∷ [])))
     + total (∃.el (sort3 (14 ∷ [])))
     + ∃.el (Σ0.pf (emptyScope {Integer}))
  where
    total : List Integer → Integer
    total []       = 0
    total (x ∷ xs) = x + total xs
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
