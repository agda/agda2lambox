open import Haskell.Prelude

data Unit : Type where
  unit : Unit

myId : @0 Unit → a → a
myId unit x = x

testMyId : @0 Unit → Nat
testMyId u = myId u 42

tyId : @0 Unit → Type → Type
tyId unit a = a

testTyId : ∀ {@0 u : Unit} → tyId u (tyId u Integer → tyId u Integer)
testTyId {unit} n = n

data Tree : Type where
  Tip : Tree
  Bin : Tree → Tree → Tree

treeId : Tree → Tree
treeId Tip = Tip
treeId (Bin x y) = Bin (treeId x) (treeId y)

testTreeId : Tree → Tree
testTreeId = treeId

test : Nat
test = testMyId unit + size (testTreeId (Bin (Bin Tip Tip) Tip)) + const 0 (testTyId {unit})
  where
    size : Tree → Nat
    size Tip       = 1
    size (Bin l r) = size l + size r
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 45
_ = refl
