data Bool : Set where
  false true : Bool

infixr 7 _::_

data List (A : Set) : Set where
  []   : List A
  _::_ : A → List A → List A

filter : {A : Set} → (A → Bool) → List A → List A
filter f [] = []
filter f (x :: xs) with f x
... | true  = x :: filter f xs
... | false = filter f xs

xs : List Bool
xs = false :: true :: false :: []

test : List Bool
test = filter (λ b → b) xs
{-# COMPILE AGDA2LAMBOX test #-}
