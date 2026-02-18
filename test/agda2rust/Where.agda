open import Agda.Builtin.Nat using (Nat; suc)

-- ** Skip pattern lambdas and with-generated functions.
data II : Set where
  ◇ ◆ : II

ii ii' : II
ii  = ◇
ii' = ◆

II→II : II → II
II→II ◇ = ◆
II→II ◆ = ◇

II→II' : II → II
II→II' = λ where
  ◇ → ◆
  ◆ → ◇

II→II'' : II → II
II→II'' x with x
... | ◇ = ◆
... | ◆ = ◇

II→II''' : II → II
II→II''' x with II→II x
... | ◇ = ◆
... | ◆ = ◇

testII : Nat
testII
  with s ← ii
  with II→II s | II→II' s | II→II'' s | II→II''' s
... | ◆ | ◆ | ◆ | ◇ = 42
... | _ | _ | _ | _ = 0

-- ** Do not erase unit-like types/values.
data I : Set where
  ◇ : I

i : I
i = ◇

I→II : I → II
I→II ◇ = ◇

II→I : II → I
II→I _ = ◇

testI : Nat
testI with ◇ ← II→I (I→II i) = 42

-- ** Do not name-clash different `where`-clauses
f : Nat
f = go 21
  where
  go : Nat → Nat
  go 0 = 0
  go (suc n) = suc (suc (go n))

g : Nat → Nat
g = go
  where
  go : Nat → Nat
  go 0 = 0
  go (suc n) = suc (suc (go n))

h : Nat → Nat
h = go
  where
  go : Nat → Nat
  go _ = 42

open import Agda.Builtin.Nat

test : Nat
test = testII
     + testI
     + f
     + g 21
     + h 0
{-# COMPILE AGDA2LAMBOX test #-}
