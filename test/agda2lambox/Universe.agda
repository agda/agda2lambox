-- ** induction-recursion: a universe à la Tarski
--
-- codes `U` are defined together with their decoding `El`, a large elimination that
-- the constructors of `U` refer to; generic programs recurse on a code to build or
-- inspect elements of its decoding, including functions and dependent pairs whose
-- second component's type is computed from the first; typed λ□ declares `El` as an
-- alias of 𝕋
--
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

record ⊤ : Set where
  constructor tt

infixr 4 _,_
record Σ (A : Set) (B : A → Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B fst

data U : Set
El : U → Set

data U where
  unit nat : U
  π σ      : (a : U) → (El a → U) → U

El unit    = ⊤
El nat     = Nat
El (π a b) = (x : El a) → El (b x)
El (σ a b) = Σ (El a) λ x → El (b x)

vec : Nat → U → U
vec zero    _ = unit
vec (suc n) a = σ a λ _ → vec n a

default : (a : U) → El a
default unit    = tt
default nat     = 2
default (π a b) = λ x → default (b x)
default (σ a b) = default a , default (b (default a))

size : (a : U) → El a → Nat
size unit    _       = 0
size nat     n       = n
size (π a b) f       = size (b (default a)) (f (default a))
size (σ a b) (x , y) = size a x + size (b x) y

sizeDefault : U → Nat
sizeDefault a = size a (default a)

replicate : (n : Nat) → Nat → El (vec n nat)
replicate zero    _ = tt
replicate (suc n) x = x , replicate n x

test : Nat
test = size (σ nat λ n → vec n nat) (3 , 1 , 2 , 3 , tt)
     + size (π nat λ n → vec n nat) (λ n → replicate n 5)
     + size (π (π nat λ _ → nat) λ _ → nat) (λ f → f 0 + 9)
     + sizeDefault (vec 3 (σ nat λ _ → nat))
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
