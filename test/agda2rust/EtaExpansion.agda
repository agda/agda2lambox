open import Agda.Builtin.Nat using (Nat; _+_; _*_)
open import Agda.Builtin.Bool using (Bool; true; false)

-- ** top-level

add : Nat → Nat → Nat
add = _+_
-- add = λ x y → x + y
-- add(x,y) = x + y

add2l : Nat → Nat
add2l = 2 +_
-- add2l = λ x → 2 + x
-- add2l(x) = 2 + x

add2r : Nat → Nat
add2r = _+ 2
-- add2r = λ x → x + 2
-- add2r(x) = x + 2

add3 : Nat → Nat → Nat → Nat
add3 x y = (x + y) +_
-- add3 = λ x y z → (x + y) + z
-- add3(x,y,z) = (x + y) + z

-- ** inner-level

maybeAddL : Bool → Nat → Nat
maybeAddL true  = 40 +_
maybeAddL false = 10 +_
-- maybeAddL = λ b → case b of
--   true  → λ x → 40 + x
--   false → λ x → 10 + x
-- maybeAddL = λ b x → case b of
--   true  → 40 + x
--   false → 10 + x
-- maybeAddL(b,x) = if b then 40 + x else 10 + x

maybeAddLb : Bool → Nat → Nat
maybeAddLb true  = λ x → 40 + x
maybeAddLb false = λ x → 10 + x
-- maybeAddLb(b,x) = if b then 40 + x else 10 + x

maybeAddR : Bool → Nat → Nat
maybeAddR true  = _+ 40
maybeAddR false = _+ 10
-- maybeAddR(b,x) = if b then x + 40 else x + 10

maybeAddRb : Bool → Nat → Nat
maybeAddRb true  = λ x → x + 40
maybeAddRb false = λ x → x + 10
-- maybeAddRb(b,x) = if b then x + 40 else x + 10

maybeAdd : Bool → Nat → Nat → Nat
maybeAdd true  = _+_
maybeAdd false = _*_

maybeAdd3 : Bool → Nat → Nat → Nat
maybeAdd3 true  x = _+ (40 + x)
maybeAdd3 false x = _+ (10 + x)
-- maybeAdd3(b,x) = λ y → if b then y + (40 + x) else y + (10 + x)
-- maybeAdd3(b,x,y) = if b then y + (40 + x) else y + (10 + x)

maybeAdd3b : Bool → Nat → Nat → Nat
maybeAdd3b true  x y = y + (40 + x)
maybeAdd3b false x y = y + (10 + x)
-- maybeAdd3b(b,x,y) = if b then y + (40 + x) else y + (10 + x)

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just : A → Maybe A

maybeInc : Maybe Nat → Nat → Nat
maybeInc (just y) x = x + y
maybeInc nothing  x = x
-- maybeInc(m,x) = match m { Just y -> x + y, Nothing -> x }

maybeAddM : Maybe Nat → Nat → Nat → Nat
maybeAddM (just z) x y = x + y + z
maybeAddM nothing  x y = x + y
-- maybeAddM(m,x,y) = match m { Just z -> x + y + z, Nothing -> x + y }

maybeIncInc : Maybe (Maybe Nat) → Nat → Nat
maybeIncInc (just (just y)) x = x + y
maybeIncInc (just nothing)  x = x + 1
maybeIncInc nothing         x = x
-- maybeIncInc(mm,x) = match m
--   { Just m -> match m
--     { Just y -> x + y
--     , Nothing -> x + 1 }
--   , Nothing -> x }

maybeIncInc2 : Maybe (Maybe Nat) → Nat → Nat → Nat
maybeIncInc2 (just (just z)) x y = x + y + z
maybeIncInc2 (just nothing)  x y = x + y + 1
maybeIncInc2 nothing         x y = x + y
-- maybeIncInc2(mm,x,y) = match m
--   { Just m -> match m
--     { Just z -> x + y + z
--     , Nothing -> x + y + 1 }
--   , Nothing -> x + y }

-- ** non-app-free heads

{-
id : ∀ {A : Set} → A → A
id = λ x → x
-- id(x) = x

addId : Nat → Nat → Nat
addId = id _+_
-- addId = λ x y → id _+_ x y
-- ** cannot infer the arity of treeless terms..
-- addId(x,y) = id _+_ x y

addId2l : Nat → Nat
addId2l = id (2 +_)
-- addId2l = λ x → id (λ y → 2 + y) x
-- addId2l(x) = id (λ y → 2 + y) x

addId2r : Nat → Nat
addId2r = id (_+ 2)
-- addId2r = λ x → id (λ y → y + 2) x
-- addId2r(x) = id (λ y → y + 2) x

addId3 : Nat → Nat → Nat → Nat
addId3 x y = id ((x + y) +_)
-- addId3 = λ x y z → id (λ z → (x + y) + z) z
-- addId3(x,y,z) = id (λ z → (x + y) + z) z
-}

open import Agda.Builtin.Nat

test : Nat
test
  = add 40 2
  + add2l 40
  + add2r 40
  + add3 40 1 1

  + maybeAddL true 2
  + maybeAddLb true 2
  + maybeAddR true 2
  + maybeAddRb true 2
  + maybeAdd true 40 2
  + maybeAdd false 2 21
  + maybeAdd3 true 1 1
  + maybeAdd3b true 1 1

  + maybeInc (just 40) 2
  + maybeAddM (just 40) 1 1
  + maybeIncInc (just (just 40)) 2
  + maybeIncInc2 (just (just 40)) 1 1
{-# COMPILE AGDA2LAMBOX test #-}
