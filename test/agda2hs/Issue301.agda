open import Haskell.Prelude
open import Haskell.Prim.Monoid
open import Haskell.Prim.Foldable

data MyData (a : Type) : Type where
  Nuttin' : MyData a

-- notice this does not occur with other classes such as Foldable
myDataDefaultFoldable : DefaultFoldable MyData
DefaultFoldable.foldMap myDataDefaultFoldable _ _ = mempty

instance
  MyDataFoldable : Foldable MyData
  MyDataFoldable = record {DefaultFoldable myDataDefaultFoldable}

-- need to create instance for semigroup first
-- (requires explicitly typed function AFAICT)
_><_ : {a : Type} -> MyData a -> MyData a -> MyData a
_ >< _ = Nuttin'

instance
  MyDataSemigroup : Semigroup (MyData a)
  MyDataSemigroup ._<>_ = _><_

instance
  myDataDefaultMonoid : DefaultMonoid (MyData a)
  DefaultMonoid.mempty myDataDefaultMonoid = Nuttin'

instance
  MyDataMonoid : Monoid (MyData a)
  MyDataMonoid = record {DefaultMonoid myDataDefaultMonoid}

test : Nat
test = sum (Nuttin' {Nat})
     + (case Nuttin' {Nat} <> mempty of λ where Nuttin' → 42)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
