open import Agda.Builtin.Nat using (_+_) renaming (Nat to ℕ)

record Foo : Set where
  field foo : ℕ
        @0 bar : ℕ

getFoo : Foo → ℕ
getFoo record {foo = n} = n

_+Foo_ : Foo → Foo → Foo
a +Foo b = record
  { foo = a .Foo.foo + b .Foo.foo
  ; bar = a .Foo.bar + b .Foo.bar }

testFoo : ℕ
testFoo = (record {foo = 21; bar = 1} +Foo record {foo = 21; bar = 2}).Foo.foo

data Bar : Set where
  bar : ℕ → @0 ℕ → Bar

getBar : Bar → ℕ
getBar (bar n _) = n

_+Bar_ : Bar → Bar → Bar
(bar a0 a1) +Bar (bar b0 b1) = bar (a0 + b0) (a1 + b1)

testBar : ℕ
testBar with bar 21 1 +Bar bar 21 2
... | bar n _ = n

test : ℕ
test = testFoo
     + testBar
     + getFoo (record {foo = 42; bar = 0})
     + getBar (bar 42 0)
{-# COMPILE AGDA2LAMBOX test #-}
