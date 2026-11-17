open import Haskell.Prelude

data D1 (t : Type → Type) : Type where
  C1 : t Bool → D1 t

f1 : D1 (λ a → Integer → a)
f1 = C1 (_== 0)

data D2 (t : Type → Type → Type) : Type where
  C2 : t Integer Integer → D2 t

f2 : D2 (λ a b → a → b)
f2 = C2 (_+ 1)

test : Integer
test = case f1 of λ where
  (C1 h) → case f2 of λ where
    (C2 g) → g (if h 0 then 41 else 0)
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 42
_ = refl
