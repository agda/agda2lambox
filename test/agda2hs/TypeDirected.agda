{-# OPTIONS --prop #-}
module TypeDirected where

open import Agda.Builtin.Reflection hiding (Type)
open import Agda.Builtin.Unit
open import Haskell.Prelude

data MyProp : Prop where
  true : MyProp

myconst : {a : Type} → MyProp → a → {y : a} → a
myconst p x = x

defTrue : Term → TC ⊤
defTrue hole = unify hole (quoteTerm True)

fn : {@(tactic defTrue) b : Bool} → Integer
fn {False} = 0
fn {True } = 5

test1 : Integer
test1 = fn

test2 : Integer
test2 = fn {False}

test : Integer
test = test1 + test2 + myconst true 37 {0}
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
