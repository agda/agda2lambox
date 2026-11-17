open import Haskell.Prim using (Type)

data Nat : Type where
  Zero : Nat
  Suc  : Nat → Nat

Nat' = Nat

myNat : Nat'
myNat = Suc (Suc Zero)

data List (a : Type) : Type where
  Nil : List a
  Cons : a → List a → List a

List' : Type → Type
List' a = List a

NatList = List Nat

myListFun : List' Nat' → NatList
myListFun Nil = Nil
myListFun (Cons x xs) = Cons x (myListFun xs)

ListList : Type → Type
ListList a = List (List a)

flatten : ∀ {a} → ListList a → List a
flatten Nil = Nil
flatten (Cons Nil xss) = flatten xss
flatten (Cons (Cons x xs) xss) = Cons x (flatten (Cons xs xss))

open import Agda.Builtin.Equality

test : Nat
test = sum (flatten (Cons (myListFun (Cons myNat Nil)) (Cons Nil (Cons (Cons (Suc Zero) Nil) Nil))))
  where
    _+_ : Nat → Nat → Nat
    Zero  + n = n
    Suc m + n = Suc (m + n)

    sum : List Nat → Nat
    sum Nil         = Zero
    sum (Cons x xs) = x + sum xs
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ Suc (Suc (Suc Zero))
_ = refl
