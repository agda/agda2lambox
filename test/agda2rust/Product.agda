-- ** no unused type parameters
record _×_ (A B : Set) : Set where
  constructor _,_
  field proj₁ : A
        proj₂ : B
open _×_ public

private variable A B C : Set

mapFst : (A → C) → A × B → C × B
mapFst f (a , b) = f a , b

mapSnd : (B → C) → A × B → A × C
mapSnd f (a , b) = a , f b

fst : A × B → A
fst = proj₁

open import Agda.Builtin.Nat

test : Nat
test = mapSnd (_+ 1) (0 , 20) .proj₂ + mapFst (_+ 2) (19 , 0) .proj₁
{-# COMPILE AGDA2LAMBOX test #-}
