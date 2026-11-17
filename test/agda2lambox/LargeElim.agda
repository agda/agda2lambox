-- ** large elimination
--
-- types computed by recursion on values: the arity of a variadic function, vectors
-- as nested pairs, and the type of a value chosen by a tag; each matches on a
-- value, so typed λ□ declares it as an alias of 𝕋
--
open import Agda.Builtin.Nat
open import Agda.Builtin.Bool
open import Agda.Builtin.Equality

record ⊤ : Set where
  constructor tt

infixr 4 _,_
record _×_ (A B : Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B

Nary : Nat → Set
Nary zero    = Nat
Nary (suc n) = Nat → Nary n

sumN : (n : Nat) → Nat → Nary n
sumN zero    acc = acc
sumN (suc n) acc = λ x → sumN n (acc + x)

Vec : Set → Nat → Set
Vec A zero    = ⊤
Vec A (suc n) = A × Vec A n

replicate : {A : Set} (n : Nat) → A → Vec A n
replicate zero    _ = tt
replicate (suc n) x = x , replicate n x

foldr : {A B : Set} (n : Nat) → (A → B → B) → B → Vec A n → B
foldr zero    _ z _        = z
foldr (suc n) f z (x , xs) = f x (foldr n f z xs)

data Tag : Set where
  nat bool fun : Tag

⟦_⟧ : Tag → Set
⟦ nat  ⟧ = Nat
⟦ bool ⟧ = Bool
⟦ fun  ⟧ = Nat → Nat

default : (t : Tag) → ⟦ t ⟧
default nat  = 2
default bool = true
default fun  = suc

toNat : (t : Tag) → ⟦ t ⟧ → Nat
toNat nat  n     = n
toNat bool true  = 1
toNat bool false = 0
toNat fun  f     = f 1

test : Nat
test = sumN 3 0 10 11 1
     + foldr 3 _+_ 0 (replicate 3 5)
     + toNat nat (default nat) + toNat bool (default bool) + toNat fun (default fun)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
