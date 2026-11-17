open import Haskell.Prelude

module ScopedTypeVariables (@0 x : Bool) where

-- We can encode explicit `forall` quantification by module parameters in Agda.
module _ {a : Type} {{_ : Eq a}} where
  foo : a → Bool
  foo x = it x == x
    where
      it : a -> a
      it = const x

-- Type arguments should be compiled in the right order.
module _ {a b : Type} where
  bar : a → b → (b → b) → b
  bar x y f = baz y
    where
      baz : b → b
      baz z = f (f z)

data D : Type where
  MakeD : (y : Bool) → @0 x ≡ y → D

mybool : Bool
mybool = False

test : Nat
test = (if foo {Nat} 1 then 30 else 0) + bar True 10 (_+ 1) + (if mybool then 1 else 0)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
