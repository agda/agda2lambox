open import Haskell.Prelude

-- Example from http://learnyouahaskell.com/a-fistful-of-monads#getting-our-feet-wet-with-maybe

Birds = Integer
Pole = Birds × Birds

landLeft : Birds → Pole → Maybe Pole
landLeft n (left , right) =
  if abs ((left + n) - right) < 4
  then Just (left + n , right)
  else Nothing

landRight : Birds → Pole → Maybe Pole
landRight n (left , right) =
  if abs (left - (right + n)) < 4
  then Just (left , right + n)
  else Nothing

routine : Maybe Pole
routine = do
    start ← return (0 , 0)
    first ← landLeft 2 start
    second ← landRight 2 first
    landLeft 1 second

routineWithoutDo : Maybe Pole
routineWithoutDo =
    return (0 , 0) Dont.>>= λ start →
    landLeft 2 start Dont.>>= λ first →
    landRight 2 first Dont.>>= λ second →
    landLeft 1 second

swapPolesMaybe : Maybe Pole → Maybe Pole
swapPolesMaybe x = do
  (one , two) ← x
  pure (two , one)

test : Integer
test = maybe 0 (uncurry _+_) routine
     + maybe 0 (uncurry _+_) routineWithoutDo
     + maybe 0 fst (swapPolesMaybe routine)
     + maybe 1 (uncurry _+_) (landLeft 10 (0 , 0))
{-# COMPILE AGDA2LAMBOX test #-}

_ : test ≡ 13
_ = refl
