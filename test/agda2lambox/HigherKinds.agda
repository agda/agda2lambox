-- ** higher-kinded types
--
-- parameters and type variables of kind `Set → Set`, instantiated with type
-- constructors, a partially applied one (`Either E`), a type scheme (`Pair`), a
-- type-level λ and a composition, and one of kind `(Set → Set) → Set → Set`;
-- as MetaRocq's erasure does, typed λ□ drops the arguments of an applied type
-- variable (`F A` ↦ `F`), and an argument of kind `Set → Set` that is not a type
-- variable is 𝕋 (`Functor List`)
--
-- NB: `bump` and `liftAll` are recursive, so that the treeless translation, which
-- unfolds copattern definitions and projection-like functions, keeps the instances
--
open import Agda.Builtin.Nat
open import Agda.Builtin.List
open import Agda.Builtin.Equality

infixr 4 _,_
record _×_ (A B : Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A → Maybe A

data Either (A B : Set) : Set where
  left  : A → Either A B
  right : B → Either A B

record Functor (F : Set → Set) : Set₁ where
  field fmap : {A B : Set} → (A → B) → F A → F B
open Functor

record Compose (F G : Set → Set) (A : Set) : Set where
  constructor compose
  field decompose : F (G A)
open Compose

Pair : Set → Set
Pair A = A × A

StateT : Set → (Set → Set) → Set → Set
StateT S M A = S → M (A × S)

record MonadTrans (T : (Set → Set) → Set → Set) : Set₁ where
  field lift : {M : Set → Set} {A : Set} → Functor M → M A → T M A
open MonadTrans

map : {A B : Set} → (A → B) → List A → List B
map f []       = []
map f (x ∷ xs) = f x ∷ map f xs

functorList : Functor List
functorList .fmap = map

functorMaybe : Functor Maybe
functorMaybe .fmap f nothing  = nothing
functorMaybe .fmap f (just x) = just (f x)

functorEither : {E : Set} → Functor (Either E)
functorEither .fmap f (left e)  = left e
functorEither .fmap f (right x) = right (f x)

functorPair : Functor Pair
functorPair .fmap f (x , y) = f x , f y

functorReader : Functor (λ A → Nat → A)
functorReader .fmap f g n = f (g n)

functorCompose : {F G : Set → Set} → Functor F → Functor G → Functor (Compose F G)
functorCompose φ ψ .fmap f (compose x) = compose (φ .fmap (ψ .fmap f) x)

transStateT : {S : Set} → MonadTrans (StateT S)
transStateT .lift φ m s = φ .fmap (λ a → a , s) m

bump : {F : Set → Set} → Functor F → Nat → F Nat → F Nat
bump φ zero    xs = xs
bump φ (suc n) xs = φ .fmap suc (bump φ n xs)

liftAll : {T : (Set → Set) → Set → Set} {M : Set → Set} {A : Set}
        → MonadTrans T → Functor M → List (M A) → List (T M A)
liftAll τ φ []       = []
liftAll τ φ (m ∷ ms) = τ .lift φ m ∷ liftAll τ φ ms

sum : List Nat → Nat
sum []       = 0
sum (x ∷ xs) = x + sum xs

fromMaybe : Maybe Nat → Nat
fromMaybe nothing  = 0
fromMaybe (just n) = n

fromEither : Either Nat Nat → Nat
fromEither (left n)  = n
fromEither (right n) = n

total : Pair Nat → Nat
total (x , y) = x + y

test : Nat
test = sum (bump functorList 2 (1 ∷ 2 ∷ []))
     + fromMaybe (bump functorMaybe 3 (just 3))
     + fromEither (bump functorEither 1 (right 4))
     + total (bump functorPair 1 (1 , 2))
     + bump functorReader 2 (λ n → n + n) 2
     + sum (map fromMaybe (decompose (bump (functorCompose functorList functorMaybe) 1
                                        (compose (just 1 ∷ nothing ∷ just 2 ∷ [])))))
     + sum (map (λ t → fromMaybe (functorMaybe .fmap total (t 3)))
                (liftAll transStateT functorMaybe (just 5 ∷ nothing ∷ [])))
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
