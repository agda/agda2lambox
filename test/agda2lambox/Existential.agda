-- ** existential types
--
-- constructors that hide a type (relevant or erased), a type constructor or an
-- index, a record with a type field (an abstract type), and the Church encoding of
-- an existential (rank-2); λ□ keeps a hidden type as a □ field
--
open import Agda.Builtin.Nat
open import Agda.Builtin.Bool
open import Agda.Builtin.Equality

not : Bool → Bool
not true  = false
not false = true

fromBool : Bool → Nat
fromBool true  = 1
fromBool false = 0

data Obs : Set₁ where
  obs : {A : Set} → A → (A → Nat) → Obs

observe : Obs → Nat
observe (obs x f) = f x

data Obs⁰ : Set₁ where
  obs⁰ : {@0 A : Set} → A → (A → Nat) → Obs⁰

observe⁰ : Obs⁰ → Nat
observe⁰ (obs⁰ x f) = f x

infixr 5 _∷_
data Vec (A : Set) : Nat → Set where
  []  : Vec A 0
  _∷_ : {n : Nat} → A → Vec A n → Vec A (suc n)

data SomeVec (A : Set) : Set where
  some : {@0 n : Nat} → Vec A n → SomeVec A

length : {A : Set} → SomeVec A → Nat
length (some [])       = 0
length (some (_ ∷ xs)) = suc (length (some xs))

data Sized : Set₁ where
  sized : {F : Set → Set} → F Nat → ({A : Set} → F A → Nat) → Sized

size : Sized → Nat
size (sized xs len) = len xs

record Counter : Set₁ where
  field
    State : Set
    start : State
    tick  : State → State
    read  : State → Nat
open Counter

natCounter boolCounter : Counter
natCounter  = record {State = Nat;  start = 0;     tick = suc; read = λ n → n}
boolCounter = record {State = Bool; start = false; tick = not; read = fromBool}

iterate : {A : Set} → Nat → (A → A) → A → A
iterate zero    f x = x
iterate (suc n) f x = f (iterate n f x)

run : Nat → Counter → Nat
run n c = c .read (iterate n (c .tick) (c .start))

∃Obs : Set₁
∃Obs = {R : Set} → ({A : Set} → A → (A → Nat) → R) → R

pack : {A : Set} → A → (A → Nat) → ∃Obs
pack x f k = k x f

unpack : ∃Obs → Nat
unpack e = e λ x f → f x

test : Nat
test = observe (obs 3 suc) + observe⁰ (obs⁰ true fromBool)
     + size (sized {F = SomeVec} (some (1 ∷ 2 ∷ 3 ∷ [])) length)
     + run 10 natCounter + run 3 boolCounter
     + unpack (pack 22 suc)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
