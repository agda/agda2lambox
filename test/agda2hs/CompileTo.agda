open import Haskell.Prelude
open import Haskell.Extra.Refinement
open import Haskell.Law.Equality

private variable
    @0 n : Nat

data Vec (a : Set) : @0 Nat → Set where
    [] : Vec a 0
    _∷_ : a → Vec a n → Vec a (suc n)

test1 : Vec Integer 3
test1 = 1 ∷ 2 ∷ 3 ∷ []

listToVec : (xs : List a) → Vec a (lengthNat xs)
listToVec [] = []
listToVec (x ∷ xs) = x ∷ listToVec xs

vecToList : Vec a n → List a
vecToList [] = []
vecToList (x ∷ xs) = x ∷ vecToList xs

test2 : Vec Integer 3
test2 = listToVec (map (_+ 1) (vecToList (1 ∷ 2 ∷ 3 ∷ [])))

llength : List a → Nat
llength [] = 0
llength (x ∷ xs) = 1 + llength xs

vlength : Vec a n → ∃ Nat (_≡ n)
vlength [] = 0 ⟨ refl ⟩
vlength (x ∷ xs) =
  let n ⟨ eq ⟩ = vlength xs
  in  (1 + n) ⟨ cong suc eq ⟩

test3 : ∃ Nat (_≡ 3)
test3 = vlength test1

lfoldl : (b → a → b) → b → List a → b
lfoldl f v [] = v
lfoldl f v (x ∷ xs) = lfoldl f (f v x) xs

vfoldl : (b : @0 Nat → Set) → (∀ {@0 n} → b n → a → b (suc n)) → b 0 → Vec a n → b n
vfoldl b f v [] = v
vfoldl b f v (x ∷ xs) = vfoldl (λ n → b (suc n)) f (f v x) xs

vreverse : Vec a n → Vec a n
vreverse {a = a} = vfoldl (Vec a) (λ xs x → x ∷ xs) []

test : Nat
test = llength (vecToList test1)
     + llength (vecToList test2)
     + value test3
     + lfoldl (λ n x → 10 * n + x) 0 (vecToList (vreverse (listToVec (2 ∷ 3 ∷ []))))
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 41
_ = refl
