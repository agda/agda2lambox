module StrictLet where

open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

{-# TERMINATING #-}
countdown : Nat → Nat → Nat
countdown acc zero = acc
countdown acc n    = countdown (suc acc) (n - 1)
-- ^ overlapping clause: its body will be hoisted above the pattern match,
--   making the program loop forever in a call-by-value language like λ□.

_ : countdown 0 3 ≡ 3
_ = refl

test : Nat
test = countdown 0 3
{-# COMPILE AGDA2LAMBOX test #-}
