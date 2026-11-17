-- ** rank-2 polymorphism
--
-- polymorphic functions as arguments, results and record fields, instantiated at
-- several types, among them a function type (Church exponentiation); λ□ passes
-- every type argument as □, whether explicit, implicit or erased
--
open import Agda.Builtin.Nat
open import Agda.Builtin.Bool
open import Agda.Builtin.List
open import Agda.Builtin.Equality

Church : Set₁
Church = {A : Set} → (A → A) → A → A

two three : Church
two   s z = s (s z)
three s z = s (s (s z))

_⊕_ _⊗_ _^_ : Church → Church → Church
(m ⊕ n) s z = m s (n s z)
(m ⊗ n) s   = m (n s)
m ^ n       = n m

toNat : Church → Nat
toNat n = n suc 0

fromBool : Bool → Nat
fromBool true  = 1
fromBool false = 0

length : {A : Set} → List A → Nat
length []       = 0
length (_ ∷ xs) = suc (length xs)

both : ({A : Set} → List A → Nat) → List Nat → List Bool → Nat
both f ns bs = f ns + f bs

record Sized : Set₁ where
  field size : {A : Set} → List A → Nat
open Sized

listSized : Sized
listSized .size = length

sizes : Sized → List Nat → List Bool → Nat
sizes s ns bs = s .size ns + s .size bs

explicit : ((A : Set) → A → A) → Nat
explicit f = f Nat 10 + fromBool (f Bool true)

erased : ({@0 A : Set} → A → A) → Nat
erased f = f 5 + fromBool (f true)

rank3 : (({@0 A : Set} → A → A) → Nat) → Nat
rank3 k = k λ x → x

select : Bool → {A : Set} → A → A → A
select true  x _ = x
select false _ y = y

test : Nat
test = toNat ((three ^ two) ⊕ (two ⊗ three))
     + both length (1 ∷ 2 ∷ []) (true ∷ [])
     + sizes listSized (5 ∷ 6 ∷ 7 ∷ []) (false ∷ [])
     + explicit (λ _ x → x) + rank3 erased
     + select false 1 2 + fromBool (select true true false)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
