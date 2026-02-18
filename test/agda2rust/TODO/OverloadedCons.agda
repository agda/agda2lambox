open import Agda.Builtin.Nat using (Nat)

open import Agda.Builtin.String using (String)

Email     = String
Password  = String
AccountNo = Nat

data User : Set where
  mk : Email → Password → AccountNo → User

accountNo : User → Nat
accountNo (mk _ _ n) = n

exUser : User
exUser = mk "xxx@xxx.com" "qwerty" 42

Road    = String
RoadNo  = Nat
Town    = String
Country = String

data Address : Set where
  mk : Road → RoadNo → Town → Country → Address

roadNo : Address → Nat
roadNo (mk _ n _ _) = n

exAddress : Address
exAddress = mk "Bay Loan" 42 "Morpeth" "UK"

open import Agda.Builtin.Nat

test : Nat
test = accountNo exUser
     + roadNo exAddress
{-# COMPILE AGDA2LAMBOX test #-}
