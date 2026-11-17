module TypeOperatorImport where

open import Haskell.Prelude hiding (_<_)
open import TypeOperatorExport

test1 : ⊤ < Bool
test1 = tt

test2 : Bool -> Bool -> ⊤ *** Bool
test2 b1 b2 = ((tt :*:_) ∘ not) (b1 &&& b2)

test : Nat
test = go test1 (test2 True True) + go test1 (test2 False True)
  where
    go : ⊤ → ⊤ *** Bool → Nat
    go tt (_ :*: b) = if b then 2 else 40
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
